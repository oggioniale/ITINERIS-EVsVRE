# once in the package, remove library imports from here.
# library(httr)
# library(jqr)
# library(dplyr)
# library(tibble)
# library(tidyr)
# library(digest)
# library(jsonlite)

# definizione funzione ------

#' Create a CSW records query function with spatial/attribute filters
#'
#' @description This factory function generates a customized function to query a CSW (Catalogue
#' Service for the Web) endpoint that supports JSON output. The returned function
#' caches the results on disk and in memory and applies optional client-side filters
#' on bounding box, keyword, and temporal range.
#'
#' The returned function will:
#' * Download CSW records in JSON format from the specified endpoint;
#' * Use a \code{jq} expression via \code{jqr} to extract metadata fields, including
#'   the bounding box split into numeric \code{xmin}, \code{ymin}, \code{xmax}, \code{ymax};
#' * Cache the full dataset on disk to avoid repeated downloads;
#' * Apply optional spatial, keyword, and date filters to the cached data.
#'
#' @param cache_dir Character. Directory where serialized cache files are stored.
#'   Defaults to the temporary directory of the R session.
#' @param maxRecords Integer. Maximum number of records to retrieve from the CSW
#'   endpoint in a single request (used in the \code{GetRecords} call).
#'
#' @return A function with signature
#'   \code{function(bbox_filter = NULL, keyword = NULL, date_range = NULL, force_update = FALSE)}
#'   that returns a \code{data.frame} of CSW records matching the specified filters.
#'
#' @details
#' The returned function supports the following optional arguments:
#' * \code{bbox_filter} – a named numeric vector with elements \code{xmin, ymin, xmax, ymax}
#'   defining a bounding box to spatially filter records.
#' * \code{keyword} – a character string used to match against record titles and abstracts.
#' * \code{date_range} – a two-element character vector \code{c("YYYY-MM-DD","YYYY-MM-DD")}
#'   to filter records by date.
#' * \code{force_update} – logical; if \code{TRUE}, ignores the existing cache and
#'   performs a fresh download from the CSW endpoint.
#'
#' @examples
#' \dontrun{
#' # Create query function for a CSW endpoint
#' getDARRecords <- make_getRecords_bbox_jqr()
#'
#' # Get all records
#' all_records <- getDARRecords()
#'
#' # Apply a spatial filter
#' bbox_filter <- c(xmin = 12.30, ymin = 45.34, xmax = 12.31, ymax = 45.35)
#' filtered <- getDARRecords(bbox_filter = bbox_filter)
#'
#' # Apply a keyword search too
#' keyword_results <- getDARRecords(keyword = "temperature")
#'
#' # Filter by date
#' date_result <- getDARRecords(date_range = c("2025-01-01", "2025-12-31"))
#' 
#' # all filters
#' records_all <- getDARRecords(
#'   bbox_filter = bbox_filter,
#'   keyword = "temperature",
#'   date_range = c("2025-01-01", "2025-12-31")
#' )
#' }
#' @author Paolo Tagliolato
#' @export
#' @importFrom httr GET content
#' @importFrom jqr jq
#' @importFrom dplyr filter mutate rowwise ungroup
#' @importFrom tibble tibble
#' @importFrom tidyr unnest
#' @importFrom digest digest
#' @importFrom jsonlite stream_in
make_getRecords_bbox_jqr <- function(#csw_url,
                                     cache_dir = tempdir(),
                                     maxRecords = 1000) {
  
  csw_url<-"https://dataregistry.lteritalia.it/catalogue/csw"
  api_cnr<-"https://dataregistry.lteritalia.it/api/v2/cnr/"

  j2df <- function(x){
    textConnection(x) %>% jsonlite::stream_in(simplifyDataFrame = TRUE)
  }
  
  get_cnr_record_EV_info<-function(id){
    url <- paste0(api_cnr, id)
    message(url)
    
    export <- httr::GET(url)
    jj <- httr::content(export, "text", encoding = "UTF-8")
    
    # substitute .label with .id to retrieve (non-standard?) identifiers of EVs.
    # the textual labels are easy to decompose in order to align them with
    # EVs currently in use.
    q<-'{
          id: .identifier,
          ECVs: (
            ((.variables.ECVsReference // [])
             | map(select(has("id")) | .label)
             | join(";")) as $v
            | if $v == "" then null else $v end
          ),
          EBVs: (
            ((.variables.EBVsReference // [])
             | map(select(has("id")) | .label)
             | join(";")) as $v
            | if $v == "" then null else $v end
          )
        }'
    
    jj %>% jqr::jq(q) %>% j2df() %>% as_tibble()
  }

  safe_name <- paste0("csw_cache_jqr_", digest(csw_url, algo = "sha1"), ".rds")
  cache_file <- file.path(cache_dir, safe_name)
  cached_records <- NULL

  function(bbox_filter = NULL,
           keyword = NULL,
           date_range = NULL,
           force_update = FALSE) {

    cnr_info<-TRUE
    # --- 1. Carica dalla cache se esiste
    if (!force_update && !is.null(cached_records)) {
      records_df <- cached_records
    } else if (!force_update && file.exists(cache_file)) {
      message("Carico i record dalla cache su disco...")
      records_df <- readRDS(cache_file)
      cached_records <<- records_df
    } else {
      message("Scarico i record dal CSW in JSON...")
      nextRecord<-1
      records_df <- NULL # initial assigment
      setname="summary" # it could be full. See q_full for reference if the full MD record should be returned in the future.
      while(nextRecord>0){
        url <- paste0(csw_url,
                      "?service=CSW&version=2.0.2&request=GetRecords",
                      "&resultType=results&elementSetName=",setname,
                      "&typenames=csw:Record&maxRecords=", maxRecords,
                      "&startPosition=",nextRecord,
                      "&outputFormat=application/json")
        message(url)
  
        export <- httr::GET(url)
        jj <- httr::content(export, "text", encoding = "UTF-8")
        # message(jj)
        q_nextRecord <-'.["csw:GetRecordsResponse"]["csw:SearchResults"]["@nextRecord"]'
        nextRecord <- jj %>% jqr::jq(q_nextRecord) %>% j2df() %>% .[1,1]
        # ["@numberOfRecordsMatched"]
        # ["@numberOfRecordsReturned"]
        # --- 2. Query jq con estrazione diretta di xmin, ymin, xmax, ymax
        # q_full <- '
        # .["csw:GetRecordsResponse"]["csw:SearchResults"]["csw:Record"][] |
        # {
        #   id: .["dc:identifier"],
        #   title: .["dc:title"],
        #   type: .["dc:type"],
        #   subject: (.["dc:subject"][] | select(.["@scheme"]=="http://www.isotc211.org/2005/resources/Codelist/gmxCodelists.xml#MD_TopicCategoryCode") | .["#text"]),
        #   format: .["dc:format"],
        #   download: (.["dct:references"][] | select(.["#text"] | endswith("download")) | .["#text"]),
        #   modified: .["dct:modified"],
        #   abstract: .["dct:abstract"],
        #   date: .["dc:date"],
        #   creator: .["dc:creator"],
        #   language: .["dc:language"],
        #   xmin: (.["ows:BoundingBox"]["ows:LowerCorner"] | split(" ") | .[1] | tonumber),
        #   ymin: (.["ows:BoundingBox"]["ows:LowerCorner"] | split(" ") | .[0] | tonumber),
        #   xmax: (.["ows:BoundingBox"]["ows:UpperCorner"] | split(" ") | .[1] | tonumber),
        #   ymax: (.["ows:BoundingBox"]["ows:UpperCorner"] | split(" ") | .[0] | tonumber)
        # }
        # '
        
        # q for summary record
        q <- '
        .["csw:GetRecordsResponse"]["csw:SearchResults"]["csw:SummaryRecord"][] |
        {
          id: .["dc:identifier"],
          title: .["dc:title"],
          type: .["dc:type"],
          subject: (
            .["dc:subject"]
            | map(
                if type == "string" then .
                elif type == "object" and has("#text") then .["#text"]
                else empty
                end
              )
            | join(",")
          ),
          format: .["dc:format"],
          download: (.["dct:references"][] | select(.["#text"] | endswith("download")) | .["#text"]),
          modified: .["dct:modified"],
          abstract: .["dct:abstract"],
          xmin: (.["ows:BoundingBox"]["ows:LowerCorner"] | split(" ") | .[1] | tonumber),
          ymin: (.["ows:BoundingBox"]["ows:LowerCorner"] | split(" ") | .[0] | tonumber),
          xmax: (.["ows:BoundingBox"]["ows:UpperCorner"] | split(" ") | .[1] | tonumber),
          ymax: (.["ows:BoundingBox"]["ows:UpperCorner"] | split(" ") | .[0] | tonumber)
        }
        '
  
        # --- 3. Estrai dati con jqr + j2df
        if(!is.null(records_df)){
          records_df <- rbind(records_df, jj %>% jqr::jq(q) %>% j2df() %>% as_tibble())
        } else {
          records_df <- jj %>% jqr::jq(q) %>% j2df() %>% as_tibble()
        }
      }

      # --- 3. harvest cnr api
      
      
      if(cnr_info){
        #for(id in records_df %>% pull(id)){
        ev_info<-purrr::map_dfr(records_df %>% pull(id), .f = get_cnr_record_EV_info)
        records_df<-records_df %>% left_join(ev_info)
        # 
      }
      
      
      # --- 4. Salva in cache
      saveRDS(records_df, cache_file)
      cached_records <<- records_df
    }

    filtered <- records_df
    
    
    # --- 5. Filtro opzionale BBOX
    if (!is.null(bbox_filter)) {
      message("Applying spatial filter")
      filtered <- filtered %>%
        filter(
          !is.na(xmin) & !is.na(xmax) & !is.na(ymin) & !is.na(ymax) &
            xmax >= bbox_filter["xmin"] &
            xmin <= bbox_filter["xmax"] &
            ymax >= bbox_filter["ymin"] &
            ymin <= bbox_filter["ymax"]
        )
    }

    # --- 6. Filtro opzionale keyword
    if (!is.null(keyword)) {
      message("Applying keyword filter")
      keyword <- tolower(keyword)
      filtered <- filtered %>%
        filter(grepl(keyword, tolower(title)) |
                 grepl(keyword, tolower(abstract)))
    }

    # --- 7. Filtro opzionale date
    if (!is.null(date_range)) {
      message("Applying date range filter")
      start <- as.Date(date_range[1])
      end <- as.Date(date_range[2])
      filtered <- filtered %>%
        filter(
          (!is.na(date) & as.Date(date) <= end) &
            (!is.na(date) & as.Date(date) >= start)
        )
    }
    
    #---- 8. TBD. filtro opzionale E*Vs

    return(filtered)
  }
}

#' retrieve the full set of ecv and ebv keywords stored in LTER Italy DAR
#' @return `tibble` with columns type (one of "ebv" or "ecv"), id (lowercase concatenated by underscore ev name), label (ev name).
get_dar_ebv_ecv<-function(){
  
  url <- "https://dataregistry.lteritalia.it/api/thesaurus/keywords/"
  message(url)
  
  export <- httr::GET(url)
  jj <- httr::content(export, "text", encoding = "UTF-8")
  
  
  q<-'(.objects // [])[]
| select((.thesaurus_identifier // "") == "cnr_ebv" or (.thesaurus_identifier // "") == "cnr_ecv")
| {
    type: ((.thesaurus_identifier // "") | sub("^cnr_"; "")),
    id: (.about // null),
    label: (.label // null)
  }'
  
  jj %>% jqr::jq(q) %>% j2df() %>% as_tibble() %>% 
    dplyr::filter(label!="") %>% 
    dplyr::arrange(type, id) 
  
}

#' align this package internal variable EVs with the list of all considered EVs, with the keywords found in DAR.
#' @return a tibble with the full join. Joining is performed on opportune trimmed versions of labels found in both lists.
get_alignment_EVDar_EVsItineris<-function(){
  # per linkare la tabella del package memorizzata in EVs e le EV di DAR ritaglio nomi:
  EVs_itineris<-EVs %>% mutate(ev_name = stringr::str_remove(name, "^[^:]*:\\s*"), .after=name)
  
  ev_dar <- get_dar_ebv_ecv() %>% dplyr::mutate(ev_name = stringr::str_remove(label, "^(?:[^:]*:\\s*|(?:EBV|ECV)\\s*-\\s*)"), .before=id) %>% dplyr::select(type, ev_name, label)
  
  ev_dar_itineris<-ev_dar %>% dplyr::full_join(EVs_itineris, by = c(ev_name="ev_name"))
  
  
  # rimane una incongruenza sulla variabile EBV - Ecosystem function: Marine primary productivity di DAR
  # che dovrebbe (secondo EuropaBON) chiamarsi "Marine ecosystem productivity" come avviene in ITINERIS.EVsVRE:::EVs
  # per il momento vanno linkati a mano. Se utente sceglie Ev marine ecosystem prod. va cercato in DAR marine primary productivity.
  # TO BE CHECKED.
  ev_dar_itineris
}
# 
# strsplit(bbv$label[1], "-") %>% unlist()
# bbv %>% 
# dplyr::mutate(lab_red=strsplit(label,"- "))
#ev_name=case_when())

# TODO: note. rimuovere da release
# # records_df %>% 
# str2match<-"Ocean, Biogeochemical: Nutrients" #EVs %>% dplyr::filter(type=="ECV") %>% pull(name) %>% .[1]
# str2match<-EVs %>% dplyr::filter(type=="ECV") %>% pull(name) %>% .[13]
# records_df %>% dplyr::filter(grepl(str2match, ECVs)|grepl(str2match,EBVs))
# 
# ebv_names_in_dar<-records_df$EBVs %>% 
#   strsplit(";",fixed = T) %>% 
#   unlist() %>% 
#   unique() %>% 
#   sort()
# 
# ecv_names_in_dar<-records_df$ECVs %>% 
#   strsplit(";",fixed = T) %>% 
#   unlist() %>% 
#   unique() %>% 
#   sort()


