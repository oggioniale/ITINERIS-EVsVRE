#' # once in the package, remove library imports from here.
#' # library(httr)
#' # library(jqr)
#' # library(dplyr)
#' # library(tibble)
#' # library(tidyr)
#' # library(digest)
#' # library(jsonlite)
#' 
#' # definizione funzione ------
#' 
#' #' Create a CSW records query function with spatial/attribute filters
#' #'
#' #' @description This factory function generates a customized function to query a CSW (Catalogue
#' #' Service for the Web) endpoint that supports JSON output. The returned function
#' #' caches the results on disk and in memory and applies optional client-side filters
#' #' on bounding box, keyword, and temporal range.
#' #'
#' #' The returned function will:
#' #' * Download CSW records in JSON format from the specified endpoint;
#' #' * Use a \code{jq} expression via \code{jqr} to extract metadata fields, including
#' #'   the bounding box split into numeric \code{xmin}, \code{ymin}, \code{xmax}, \code{ymax};
#' #' * Cache the full dataset on disk to avoid repeated downloads;
#' #' * Apply optional spatial, keyword, and date filters to the cached data.
#' #'
#' #' @param csw_url Character. The base URL of the CSW endpoint (e.g.,
#' #'   \code{"https://dataregistry.lteritalia.it/catalogue/csw"}).
#' #' @param cache_dir Character. Directory where serialized cache files are stored.
#' #'   Defaults to the temporary directory of the R session.
#' #' @param maxRecords Integer. Maximum number of records to retrieve from the CSW
#' #'   endpoint in a single request (used in the \code{GetRecords} call).
#' #'
#' #' @return A function with signature
#' #'   \code{function(bbox_filter = NULL, keyword = NULL, date_range = NULL, force_update = FALSE)}
#' #'   that returns a \code{data.frame} of CSW records matching the specified filters.
#' #'
#' #' @details
#' #' The returned function supports the following optional arguments:
#' #' * \code{bbox_filter} – a named numeric vector with elements \code{xmin, ymin, xmax, ymax}
#' #'   defining a bounding box to spatially filter records.
#' #' * \code{keyword} – a character string used to match against record titles and abstracts.
#' #' * \code{date_range} – a two-element character vector \code{c("YYYY-MM-DD","YYYY-MM-DD")}
#' #'   to filter records by date.
#' #' * \code{force_update} – logical; if \code{TRUE}, ignores the existing cache and
#' #'   performs a fresh download from the CSW endpoint.
#' #'
#' #' @examples
#' #' \dontrun{
#' #' # Create query function for a CSW endpoint
#' #' csw_url <- "https://dataregistry.lteritalia.it/catalogue/csw"
#' #' getRecords <- make_getRecords_bbox_jqr(csw_url)
#' #'
#' #' # Get all records
#' #' all_records <- getRecords()
#' #'
#' #' # Apply a spatial filter
#' #' bbox_filter <- c(xmin = 12.30, ymin = 45.34, xmax = 12.31, ymax = 45.35)
#' #' filtered <- getRecords(bbox_filter = bbox_filter)
#' #'
#' #' # Apply a keyword search too
#' #' keyword_results <- getRecords(keyword = "temperature")
#' #'
#' #' # Filter by date
#' #' date_result <- getRecords(date_range = c("2025-01-01", "2025-12-31"))
#' #' }
#' #' @author Paolo Tagliolato
#' #' @export
#' #' @importFrom httr GET content
#' #' @importFrom jqr jq
#' #' @importFrom dplyr filter mutate rowwise ungroup
#' #' @importFrom tibble tibble
#' #' @importFrom tidyr unnest
#' #' @importFrom digest digest
#' #' @importFrom jsonlite stream_in
#' make_getRecords_bbox_jqr <- function(csw_url,
#'                                      cache_dir = tempdir(),
#'                                      maxRecords = 1000) {
#'   
#'   j2df <- function(x){
#'     textConnection(x) %>% jsonlite::stream_in(simplifyDataFrame = TRUE)
#'   }
#'   
#'   safe_name <- paste0("csw_cache_jqr_", digest(csw_url, algo = "sha1"), ".rds")
#'   cache_file <- file.path(cache_dir, safe_name)
#'   cached_records <- NULL
#'   
#'   function(bbox_filter = NULL,
#'            keyword = NULL,
#'            date_range = NULL,
#'            force_update = FALSE) {
#'     
#'     # --- 1. Carica dalla cache se esiste
#'     if (!force_update && !is.null(cached_records)) {
#'       records_df <- cached_records
#'     } else if (!force_update && file.exists(cache_file)) {
#'       message("Carico i record dalla cache su disco...")
#'       records_df <- readRDS(cache_file)
#'       cached_records <<- records_df
#'     } else {
#'       message("Scarico i record dal CSW in JSON...")
#'       url <- paste0(csw_url,
#'                     "?service=CSW&version=2.0.2&request=GetRecords",
#'                     "&resultType=results&elementSetName=full",
#'                     "&typenames=csw:Record&maxRecords=", maxRecords,
#'                     "&outputFormat=application/json")
#'       message(url)
#'       
#'       export <- httr::GET(url)
#'       jj <- httr::content(export, "text", encoding = "UTF-8")
#'       # message(jj)
#'       
#'       # --- 2. Query jq con estrazione diretta di xmin, ymin, xmax, ymax
#'       q <- '
#'       .["csw:GetRecordsResponse"]["csw:SearchResults"]["csw:Record"][] |
#'       {
#'         id: .["dc:identifier"],
#'         title: .["dc:title"],
#'         type: .["dc:type"],
#'         subject: (.["dc:subject"][] | select(.["@scheme"]=="http://www.isotc211.org/2005/resources/Codelist/gmxCodelists.xml#MD_TopicCategoryCode") | .["#text"]),
#'         format: .["dc:format"],
#'         download: (.["dct:references"][] | select(.["#text"] | endswith("download")) | .["#text"]),
#'         modified: .["dct:modified"],
#'         abstract: .["dct:abstract"],
#'         date: .["dc:date"],
#'         creator: .["dc:creator"],
#'         language: .["dc:language"],
#'         xmin: (.["ows:BoundingBox"]["ows:LowerCorner"] | split(" ") | .[1] | tonumber),
#'         ymin: (.["ows:BoundingBox"]["ows:LowerCorner"] | split(" ") | .[0] | tonumber),
#'         xmax: (.["ows:BoundingBox"]["ows:UpperCorner"] | split(" ") | .[1] | tonumber),
#'         ymax: (.["ows:BoundingBox"]["ows:UpperCorner"] | split(" ") | .[0] | tonumber)
#'       }
#'       '
#'       
#'       # --- 3. Estrai dati con jqr + j2df
#'       records_df <- jj %>% jqr::jq(q) %>% j2df() %>% as_tibble()
#'       
#'       # --- 4. Salva in cache
#'       saveRDS(records_df, cache_file)
#'       cached_records <<- records_df
#'     }
#'     
#'     filtered <- records_df
#'     
#'     # --- 5. Filtro opzionale BBOX
#'     if (!is.null(bbox_filter)) {
#'       message("Applying spatial filter")
#'       filtered <- filtered %>%
#'         filter(
#'           !is.na(xmin) & !is.na(xmax) & !is.na(ymin) & !is.na(ymax) &
#'             xmax >= bbox_filter["xmin"] &
#'             xmin <= bbox_filter["xmax"] &
#'             ymax >= bbox_filter["ymin"] &
#'             ymin <= bbox_filter["ymax"]
#'         )
#'     }
#'     
#'     # --- 6. Filtro opzionale keyword
#'     if (!is.null(keyword)) {
#'       message("Applying keyword filter")
#'       keyword <- tolower(keyword)
#'       filtered <- filtered %>%
#'         filter(grepl(keyword, tolower(title)) |
#'                  grepl(keyword, tolower(abstract)))
#'     }
#'     
#'     # --- 7. Filtro opzionale date
#'     if (!is.null(date_range)) {
#'       message("Applying date range filter")
#'       start <- as.Date(date_range[1])
#'       end <- as.Date(date_range[2])
#'       filtered <- filtered %>%
#'         filter(
#'           (!is.na(date) & as.Date(date) <= end) &
#'             (!is.na(date) & as.Date(date) >= start)
#'         )
#'     }
#'     
#'     return(filtered)
#'   }
#' }
#' 
#' 
#' 
#' # Esempi --------
#' if(FALSE){
#'   csw_url <- "https://dataregistry.lteritalia.it/catalogue/csw"
#'   getRecords <- make_getRecords_bbox_jqr(csw_url)
#'   
#'   # Tutti i record
#'   all_records <- getRecords(force_update = T)
#'   
#'   # Filtra per BBOX
#'   bbox_filter <- c(xmin = 12.3063, ymin = 45.347391, xmax = 12.3083, ymax = 45.349391)
#'   records_bbox <- getRecords(bbox_filter = bbox_filter)
#'   
#'   # Filtra per keyword
#'   records_keyword <- getRecords(keyword = "temperature")
#'   
#'   # Filtra per date
#'   records_date <- getRecords(date_range = c("2025-01-01", "2025-12-31"))
#'   
#'   # Tutti i filtri insieme
#'   records_all <- getRecords(
#'     bbox_filter = bbox_filter,
#'     keyword = "temperature",
#'     date_range = c("2025-01-01", "2025-12-31")
#'   )
#'   
#' }