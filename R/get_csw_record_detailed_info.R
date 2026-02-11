# # see the examples section
# library(httr)
# library(xml2)

#' retrieve detailed information on specific (itineris) geonetwork metadata record
#' @param record_id uuid of the record within the geonetwork endpoint
#' @param csw_url endpoint of the geonetwork CSW endpoint, Default is ITINERIS geonetwork endpoint.
#' @return `list` with several features of the record (e.g. title, abstract, keywords, topicCatgoryCode, bounding box).
#' @importFrom httr GET, content
#' @importFrom xml2 read_xml, xml_text, xml_find_first xml_find_all
#' @author Paolo Tagliolato
#' @export
#' @examples
#' 
#' require(ows4R)
#' require(dplyr)
#' 
#' csw_url <- "https://geonetwork.itineris.cnr.it/geonetwork/srv/ita/csw?"
#' 
#' # initialize CSW object
#' CSW<-ows4R::CSWClient$new(csw_url, "2.0.2", logger="INFO")
#' 
#' # query the CSW service w.r.t. a given bounding box, to retrieve only 
#' the records in the area of interest
#' 
#' records <- CSW$getRecords(
#'   elementSetName = "summary",
#'   constraint = "BBOX(BoundingBox, 10, 44, 15, 46)",
#'   constraintLanguage = "CQL_TEXT",
#'   maxRecords = 10
#'   )
#'   
#' length(records) # how many records here?
#' 
#' # store records in a tibble structure
#' recs_tbl<-purrr::map_dfr(records_full, .f=function(x){return (x %>% as_tibble())})
#' 
#' # choose one record (here, the first one)
#' i=1 # i is the position of the record of interest in the tibble
#' 
#' # extract the uuid of the record
#' record_id <- recs_tbl[i,"identifier"] %>% pull()
#'
#' ########### now obtain detailed info on the record ############
#' record_info <- get_csw_record_detailed_info(record_id, csw_url)
#' ###############################################################
#' 
#' # print info
#' record_info
#' 
#' # if the CSW is ITINERIS hub, download the corresponding dataset 
#' # (in this case we use "nc" extension: from recs_tbl$format we know it is a netCDF archive)
#' 
#' # choose name with path for the dataset to be stored locally
#' destination_file_with_path=paste0(tempdir(),"/", record_id, ".nc")
#' 
#' # download the file
#' download.file(recs_tbl[i,"URI"] %>% pull(),destfile = destination_file_with_path)
#' 
get_csw_record_detailed_info<-function(record_id, csw_url="https://geonetwork.itineris.cnr.it/geonetwork/srv/ita/csw?"){
  # è già noto record_id, che è uuid del record nel csw
  # è già noto csw_url <- "https://geonetwork.itineris.cnr.it/geonetwork/srv/ita/csw?"
  
  
  info_record<-list()
  
  # Costruisci la URL
  # https://geonetwork.itineris.cnr.it/geonetwork/srv/eng/csw?service=CSW&version=2.0.2&request=GetRecordById&id=415be643-7e8f-11f0-ac20-45e67655f1af
  url <- paste0(
    csw_url,
    "service=CSW&version=2.0.2&request=GetRecordById",
    "&id=", record_id,
    "&outputschema=http://www.isotc211.org/2005/gmd"
  )
  
  res <- httr::GET(url)
  doc <- xml2::read_xml(httr::content(res, as = "text", encoding = "UTF-8"))
  
  # Ora puoi fare XPath
  ns <- c(
    gmd = "http://www.isotc211.org/2005/gmd",
    gco = "http://www.isotc211.org/2005/gco"
  )
  
  xmin <- as.numeric(xml2::xml_text(xml2::xml_find_first(doc, "//gmd:EX_GeographicBoundingBox/gmd:westBoundLongitude/gco:Decimal", ns)))
  xmax <- as.numeric(xml2::xml_text(xml2::xml_find_first(doc, "//gmd:EX_GeographicBoundingBox/gmd:eastBoundLongitude/gco:Decimal", ns)))
  ymin <- as.numeric(xml2::xml_text(xml2::xml_find_first(doc, "//gmd:EX_GeographicBoundingBox/gmd:southBoundLatitude/gco:Decimal", ns)))
  ymax <- as.numeric(xml2::xml_text(xml2::xml_find_first(doc, "//gmd:EX_GeographicBoundingBox/gmd:northBoundLatitude/gco:Decimal", ns)))
  
  info_record$bbox <- c(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)
  info_record$title<-xml2::xml_text(xml2::xml_find_first(doc, "//gmd:title", ns))
  info_record$abstract <- xml2::xml_text(xml2::xml_find_first(doc, "//gmd:abstract/gco:CharacterString", ns))
  info_record$topicCategoryCode<-xml2::xml_text(xml2::xml_find_all(doc,"//gmd:MD_TopicCategoryCode"), ns)
  # qui si può essere più specifici, mi limito nell'esempio a trovare tutti gli elementi gmd:keyword
  info_record$keywords<-xml2::xml_text(xml2::xml_find_all(doc,"//gmd:keyword", ns))
  
  
  return(info_record)
}
