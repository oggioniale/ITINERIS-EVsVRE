#' Query ITINERIS GeoNetwork CSW using CQL and adaptive schema (Summary vs ISO)
#'
#' This function queries the ITINERIS GeoNetwork CSW (2.0.2) endpoint using a
#' CQL_TEXT constraint built from keywords and an optional bounding box.
#' It first performs a "hits" request to read numberOfRecordsMatched and,
#' if the number of matched records is >= threshold_full, it switches to the
#' lightweight CSW SummaryRecord schema (http://www.opengis.net/cat/csw/2.0.2).
#' Otherwise, it retrieves full ISO gmd metadata.
#'
#' Output column names follow the schema origin:
#' - Fields available in SummaryRecord use dc_* / dct_* naming
#' - Fields only available in ISO metadata use gmd_* naming
#'
#' Harmonised output columns:
#' dc_identifier, dc_title, dc_type, dct_abstract, dc_subject, dc_format,
#' gmd_extent, gmd_accessURL, gmd_topicCategoryCode
#'
#' When return_sf = TRUE and ISO schema is used, bounding boxes are converted
#' into sf polygons (EPSG:4326).
#'
#' @param keywords Character vector of keywords (used in AnyText LIKE CQL filter). 
#' If bbox is NULL, keywords must be specified, 
#' otherwise the function operates with hits=TRUE
#' @param bbox numeric vector c(xmin, ymin, xmax, ymax). 
#' If keywords is NULL, bbox must be specified, 
#' otherwise the function operates with hits=TRUE
#' @param csw_url CSW endpoint URL (default ITINERIS GeoNetwork)
#' @param return_sf Logical; if TRUE returns an sf object when extent is available
#' @param maxRecords Maximum number of records to request (default 100)
#' @param threshold_full Threshold of matched records above which SummaryRecord
#'        schema is used instead of full ISO gmd (default 100)
#' @param hits Retrieve only the number of matched records.
#' @param debug do not perform any request, just output the request url.
#' @param additionalParams named `list` for additional parameters. key-value pairs 
#' are added (or substituted) to the CSW GET request. 
#' E.g. list(elementSetName="full",typeNames="csw:Record"), 
#' useful if the user specifies a different CSW server 
#' that needs different parameters. (default list())
#' @return A `tibble` or `sf` object with harmonised metadata fields. 
#' If no record is matched on the server, the object is a tibble or sf with no rows 
#' If hits=TRUE return only the `numeric` number of matched records on the server. 
#' If neither keywords nor bbox is specified, the function automatically sets hits=TRUE.
#' If debug=TRUE, the output is a `character` with the (not perfomed) 
#' request url.
#' @author Paolo Tagliolato (ptagliolato)
#' @examples
#' # example code
#' \dontrun{
#' # return only the number of matches for a query and bbox
#' get_itineris_csw_records(
#'   hits=T, 
#'   keywords = c("turbidity", "phytoplankton"), 
#'   bbox = c(12.26, 45.42, 12.43, 45.50)
#' )
#'   
#' # return the actual records of the previous query
#' get_itineris_csw_records(
#'   keywords = c("turbidity", "phytoplankton"), 
#'   bbox = c(12.26, 45.42, 12.43, 45.50)
#'   )
#'   
#' # return the actual records of the previous query as an sf object
#' # with bounding box geometries for each record (see leaflet map example later)
#' records_sf <- get_itineris_csw_records(
#'   keywords = c("turbidity", "phytoplankton"), 
#'   bbox = c(12.26, 45.42, 12.43, 45.50),
#'   return_sf = TRUE
#'   )
#'   
#' # if a query returns more than threshold_full records, to speed up things
#' # the server is requested for a simpler schema. Many columns could be empty.
#' # No geometry is returned 
#' simpler_schema_records <- get_itineris_csw_records(
#'   keywords = c("biomass"), 
#'   bbox = c(12.26, 45.42, 12.43, 45.50),
#'   threshold_full = 20, 
#'   return_sf = TRUE)
#' 
#' "sf" %in% class(simpler_schema_records) # -> FALSE
#' 
#' # if we increase the threshold full over the number of hits,
#' # we obtain the longer record schema, and the sf.
#' # Note that we limit to 10 records here, but that the choice among 
#' # the short or the long schema is done on the number of hits.
#' records_sf <- get_itineris_csw_records(
#'   keywords = c("biomass"), 
#'   bbox = c(12.26, 45.42, 12.43, 45.50),
#'   threshold_full = 1000,
#'   maxRecords=10, 
#'   return_sf = TRUE)
#' 
#' "sf" %in% class(records_sf) # -> TRUE
#' 
#' # now we plot the results as rectangle polygons on a leaflet map with popups.
#' # 
#' if("sf" %in% class(records_sf)){
#' # Create popup content for each record
#' records_sf <- records_sf %>%
#'   mutate(
#'     popup_content = paste0(
#'       "<b>dc:title</b> ", dc_title, "<br>",
#'       "<b>dc:identifier</b> ", dc_identifier, "<br>",
#'       "<b>dc:subject</b> ", dc_subject, "<br>",
#'       "<b>dct:abstract</b> ", paste0(substr(dct_abstract,1,400), "[...]"), "<br>",
#'       "<b>dc:format</b> ", dc_format, "<br>",
#'       "<b>gmd:accessURL</b> <a href='", gmd_accessURL, "' target='_blank'>link</a>"
#'     )
#'   )
#' 
#' # Leaflet map
#' leaflet(records_sf) %>%
#'   addTiles() %>%
#'   addPolygons(
#'     color = "blue",
#'     weight = 2,
#'     fill = FALSE,
#'     popup = ~popup_content,
#'     highlightOptions = highlightOptions(
#'       color = "orange",
#'       weight = 3,
#'       bringToFront = TRUE,
#'       fillOpacity = 0.5
#'     )
#'   )
#' }
#' }
#' 
#' @importFrom httr GET content
#' @importFrom xml2 read_xml xml_find_all xml_find_first xml_text xml_attr xml_ns
#' @importFrom purrr map_chr map
#' @importFrom stringr str_squish
#' @importFrom tibble tibble
#' @importFrom sf st_polygon st_sfc st_sf
#' @importFrom utils modifyList
#' @export
get_itineris_csw_records <- function(
    keywords=NULL,
    bbox = NULL,
    csw_url = NULL,
    return_sf = FALSE,
    maxRecords = 100,
    threshold_full = 100,
    hits = FALSE,
    debug = FALSE,
    additionalParams=list()
) {
  
  if (is.null(csw_url)) {
    csw_url <- "https://geonetwork.itineris.cnr.it/geonetwork/srv/eng/csw"
  
  }
  
  if(is.null(keywords) && is.null(bbox) && hits==FALSE){
    warning("keywords and bbox are empty. 
            I request only the number of matching records, 
            setting hits=TRUE from now on")
    # backdoor 
    hits=TRUE
    if(#csw_url != "https://geonetwork.itineris.cnr.it/geonetwork/srv/eng/csw" && 
       length(additionalParams)!=0 &&
       "resultType" %in% names(additionalParams) &&
       additionalParams[["resultType"]]=="results") hits=FALSE
  }
  
  
  # ---- Internal: build CQL filter ----
  build_cql <- function(keywords, bbox = NULL) {
    if(is.null(keywords)&& is.null(bbox)){
      return("")
    }
    keyword_filters <- purrr::map_chr(keywords, function(k) {
      if(is.null(keywords)) return(NULL)
      k_trim <- stringr::str_squish(k)
      if (grepl(" ", k_trim)) {
        words <- unlist(strsplit(k_trim, " "))
        paste0(
          "(",
          paste0("AnyText LIKE '", words, "'", collapse = " AND "),
          ")"
        )
      } else {
        paste0("AnyText LIKE '", k_trim, "'")
      }
    })
    
    cql_keywords <- paste(keyword_filters, collapse = " OR ")
    
    if (!is.null(bbox)) {
      if (length(bbox) != 4) {
        stop("bbox must be c(xmin, ymin, xmax, ymax)")
      }
      
      cql_bbox <- sprintf(
        "BBOX(ows:BoundingBox,%f,%f,%f,%f)",
        bbox[1], bbox[2], bbox[3], bbox[4]
      )
      
      if(cql_keywords!=""){
        paste0("(", cql_keywords, ") AND ", cql_bbox)
      } else{
        cql_bbox
      }
      
    } else {
      cql_keywords
    }
  }
  
  cql_filter <- build_cql(keywords, bbox)
  
  
  #elementSetName=full&typeNames=csw:Record
  constr_list<-list()
  
  if(cql_filter!=""){
    constr_list<-list(
      CONSTRAINTLANGUAGE = "CQL_TEXT",
      CONSTRAINT_LANGUAGE_VERSION = "1.1.0",
      CONSTRAINT = cql_filter
    )
  }
  # ---- STEP 1: hits request (count only) ----
  params_hits <- list(
    service = "CSW",
    version = "2.0.2",
    request = "GetRecords",
    resultType = "hits",
    typeNames = "gmd:MD_Metadata"
  ) %>% 
    utils::modifyList(constr_list) %>% 
    utils::modifyList(additionalParams)
  
  
 
  if(debug){
    message("[debug] querying hits: ", httr::modify_url(csw_url, query = params_hits))
  }
  
  res_hits <- httr::GET(csw_url, query = params_hits)
  xml_hits <- httr::content(res_hits, as = "text", encoding = "UTF-8")
  doc_hits <- xml2::read_xml(xml_hits)
  ns_hits <- xml2::xml_ns(doc_hits)
  
  n_matched <- suppressWarnings(
    as.numeric(
      xml2::xml_attr(
        xml2::xml_find_first(doc_hits, ".//csw:SearchResults", ns_hits),
        "numberOfRecordsMatched"
      )
    )
  )
  
  message("Matched ", n_matched, " records.")
  
  if(hits){
    return(n_matched)
  }
  
  if (is.na(n_matched)) n_matched <- 0
  
  # ---- HARD STOP: no records matched ----
  if (n_matched == 0) {
    
    if(debug){
      message("[debug] no matches, so no other query is performed, returning dummy result")
    }
    
    empty_df <- tibble::tibble(
      dc_identifier = character(0),
      dc_title = character(0),
      dc_type = character(0),
      dct_abstract = character(0),
      dc_subject = character(0),
      dc_format = character(0),
      gmd_extent = character(0),
      gmd_accessURL = character(0),
      gmd_topicCategoryCode = character(0)
    )
    
    if (return_sf) {
      empty_sf <- sf::st_sf(
        empty_df,
        geometry = sf::st_sfc(crs = 4326)
      )
      return(empty_sf)
    } else {
      return(empty_df)
    }
  }
  
  
  use_summary <- n_matched >= threshold_full
  
  message("Downloading ", min(n_matched,maxRecords), "/", 
          n_matched," records.")
  
  output_schema <- if (use_summary) {
    "http://www.opengis.net/cat/csw/2.0.2"
  } else {
    "http://www.isotc211.org/2005/gmd"
  }
  
  # ---- STEP 2: actual records ----
  params <- list(
    service = "CSW",
    version = "2.0.2",
    request = "GetRecords",
    resultType = "results",
    typeNames = "gmd:MD_Metadata",
    outputSchema = output_schema,
    maxRecords = maxRecords
  ) %>%
    utils::modifyList(constr_list) %>% 
    utils::modifyList(additionalParams)
  
  
  
  
  if(debug){
    message("[debug] returning request url with query, no request is performed")
    return(httr::modify_url(csw_url, query = params_hits))
  }
  
  res <- httr::GET(csw_url, query = params)
  xml_txt <- httr::content(res, as = "text", encoding = "UTF-8")
  doc <- xml2::read_xml(xml_txt)
  ns <- xml2::xml_ns(doc)
  
  get_first <- function(node, xpath) {
    nd <- xml2::xml_find_first(node, xpath, ns)
    if (inherits(nd, "xml_missing")) return(NA_character_)
    xml2::xml_text(nd)
  }
  
  get_all <- function(node, xpath) {
    nds <- xml2::xml_find_all(node, xpath, ns)
    if (length(nds) == 0) return(NA_character_)
    paste(trimws(xml2::xml_text(nds)), collapse = ", ")
  }
  
  if (use_summary) {
    message("Number of matches exceeds the value of 'threshold_full' parameter.",
            "Asking for records with shorter schema http://www.opengis.net/cat/csw/2.0.2")
    
    # ---- CASE 1: CSW SummaryRecord ----
    records <- xml2::xml_find_all(doc, ".//csw:SummaryRecord", ns)
    
    dc_identifier <- purrr::map_chr(records, ~ get_first(.x, "./dc:identifier"))
    dc_title      <- purrr::map_chr(records, ~ get_first(.x, "./dc:title"))
    dc_type       <- purrr::map_chr(records, ~ get_first(.x, "./dc:type"))
    dct_abstract  <- purrr::map_chr(records, ~ get_first(.x, "./dct:abstract"))
    dc_subject    <- purrr::map_chr(records, ~ get_all(.x, "./dc:subject"))
    dc_format     <- purrr::map_chr(records, ~ get_first(.x, "./dc:format"))
    
    gmd_extent <- rep(NA_character_, length(records))
    gmd_accessURL <- rep(NA_character_, length(records))
    gmd_topicCategoryCode <- rep(NA_character_, length(records))
    
    tib_extent_list <- replicate(length(records), c(NA, NA, NA, NA), simplify = FALSE)
    
  } else {
    message("Asking for records in longer schema http://www.isotc211.org/2005/gmd")
    
    # ---- CASE 2: Full ISO gmd ----
    records <- xml2::xml_find_all(doc, ".//gmd:MD_Metadata", ns)
    
    dc_identifier <- purrr::map_chr(
      records,
      ~ get_first(.x, ".//gmd:fileIdentifier/gco:CharacterString")
    )
    
    dc_title <- purrr::map_chr(
      records,
      ~ get_first(.x, ".//gmd:identificationInfo//gmd:title/gco:CharacterString")
    )
    
    dc_type <- purrr::map_chr(
      records,
      ~ get_first(.x, ".//gmd:hierarchyLevel/gmd:MD_ScopeCode")
    )
    
    dct_abstract <- purrr::map_chr(
      records,
      ~ get_first(.x, ".//gmd:identificationInfo//gmd:abstract/gco:CharacterString")
    )
    
    dc_subject <- purrr::map_chr(
      records,
      ~ get_all(.x, ".//gmd:descriptiveKeywords//gmd:keyword//gco:CharacterString")
    )
    
    dc_format <- purrr::map_chr(
      records,
      ~ get_all(.x, ".//gmd:distributionFormat//gmd:name/gco:CharacterString")
    )
    
    gmd_accessURL <- purrr::map_chr(
      records,
      ~ get_all(.x, ".//gmd:transferOptions//gmd:URL")
    )
    
    gmd_topicCategoryCode <- purrr::map_chr(
      records,
      ~ get_all(.x, ".//gmd:topicCategory/gmd:MD_TopicCategoryCode")
    )
    
    tib_extent_list <- purrr::map(records, function(rec) {
      bbox_node <- xml2::xml_find_first(rec, ".//gmd:EX_GeographicBoundingBox", ns)
      if (inherits(bbox_node, "xml_missing")) return(c(NA, NA, NA, NA))
      
      xmin <- as.numeric(get_first(bbox_node, ".//gmd:westBoundLongitude/gco:Decimal"))
      ymin <- as.numeric(get_first(bbox_node, ".//gmd:southBoundLatitude/gco:Decimal"))
      xmax <- as.numeric(get_first(bbox_node, ".//gmd:eastBoundLongitude/gco:Decimal"))
      ymax <- as.numeric(get_first(bbox_node, ".//gmd:northBoundLatitude/gco:Decimal"))
      
      c(xmin, ymin, xmax, ymax)
    })
    
    gmd_extent <- purrr::map_chr(tib_extent_list, ~ paste(.x, collapse = ","))
  }
  
  df <- tibble::tibble(
    dc_identifier = dc_identifier,
    dc_title = dc_title,
    dc_type = dc_type,
    dct_abstract = dct_abstract,
    dc_subject = dc_subject,
    dc_format = dc_format,
    gmd_extent = gmd_extent,
    gmd_accessURL = gmd_accessURL,
    gmd_topicCategoryCode = gmd_topicCategoryCode
  )
  
  if (return_sf && !use_summary) {
    polys <- purrr::map(tib_extent_list, function(e) {
      if (any(is.na(e))) return(NA)
      sf::st_polygon(list(matrix(
        c(
          e[1], e[2],
          e[1], e[4],
          e[3], e[4],
          e[3], e[2],
          e[1], e[2]
        ),
        ncol = 2,
        byrow = TRUE
      )))
    })
    
    sfc <- sf::st_sfc(polys, crs = 4326)
    df <- sf::st_sf(df, geometry = sfc)
  }
  
  return(df)
}
