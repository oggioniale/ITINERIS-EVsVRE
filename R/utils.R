#' @title do_Q function
#' @description This function parses json content returned from an HTTP request
#' @param q a character
#' @param jj a character
#' @return The output of the function is ...
#' @author Paolo Tagliolato, PhD (2021) \email{tagliolato.p@@irea.cnr.it}
#' @importFrom jsonlite stream_in
#' @importFrom jqr jq select
#' @importFrom dplyr select
#' @importFrom dtplyr lazy_dt
#' @importFrom magrittr %>%
#' @noMd
#' @noRd
#' @keywords internal
#'
### function do_q
do_Q <- function(q, jj) {
  jj %>%
    jqr::jq(as.character(q)) %>%
    textConnection(encoding = "UTF-8") %>%
    jsonlite::stream_in(simplifyDataFrame = TRUE, verbose = FALSE) %>%
    dtplyr::lazy_dt()
}

#' internal function to retrieve json content by url
#' @noMd
#' @noRd
#' @keywords internal
#' @importFrom httr2 request req_method req_headers req_retry req_perform resp_check_status resp_body_string
get_jj <- function(url, ...){
  # Normal mode
  export <- httr2::request(base_url = url, ...) %>%
    httr2::req_method("GET") %>%
    httr2::req_headers(Accept = "application/json") %>%
    httr2::req_retry(max_tries = 3, max_seconds = 120) %>%
    httr2::req_perform()
  httr2::resp_check_status(export)
  jj <- httr2::resp_body_string(export)
  return(jj)
}

#' read dataset of different type
#' @param type `character` one of "raster", "rasterTS", "shapefile", "SOS"
#' @param path `character` for raster* and shapefile that path to the file
#' @param procedure `character` for "SOS" type, the procedure id in the remote sos
#' @param url `character` the sos endpoint url
#' @return an R object with the data. It can be of class `tbl` (tibble), `sf`, `Raster`, `RasterBrick`
#' @importFrom ReLTER get_sos_obs
#' @importFrom raster raster brick
#' @importFrom sf st_read
#' @importFrom readr read_csv read_table
#' @export
readDataset<-function(type, path=NULL, procedure=NULL, url=NULL){
  
  datasetInfo <- list(type=type, path2file=path, procedure=procedure, url=url)
  theDataset <- NULL
  tryCatch(
    expr = 
      {
        if(datasetInfo$type %in% c("SOS","raster","rasterTS","shapefile","geoJ{SON")){
          if(datasetInfo$type=="SOS"){
            message("returning SOS dataset", datasetInfo$url, datasetInfo$procedure)
            message(sprintf("ReLTER::get_sos_obs(sosURL = %s,
                                              procedure = %s)",
                            datasetInfo$url, 
                            datasetInfo$procedure
                            ))
            theDataset <- ReLTER.get_sos_obs(sosURL = datasetInfo$url,
                                              procedure = datasetInfo$procedure)
          }
          if(datasetInfo$type=="raster"){
            message("returning raster dataset")
            theDataset <- raster::raster(x = datasetInfo$path2file)
          }
          if(datasetInfo$type=="rasterTS"){
            message("returning rasterTS dataset")
            theDataset <- raster::brick(x = datasetInfo$path2file)
          }
          if(datasetInfo$type=="shapefile"){
            message("returning spatial feature dataset")
            theDataset <- sf::st_read(dsn = datasetInfo$path2file)
          }
          if(datasetInfo$type=="geoJSON"){
            message("returning spatial feature dataset")
            theDataset <- sf::st_read(dsn = datasetInfo$path2file)
          }
        } else {
          if(endsWith(datasetInfo$path2file,".csv")){# && datasetInfo$type %in% c("csv", "profile", "timeseries")){
            message("returning rectangular dataset (guess: CSV)")
            theDataset <- readr::read_csv(datasetInfo$path2file)
          }
          if(endsWith(datasetInfo$path2file,".dat")){# && datasetInfo$type=="timeseries" ){
            message("returning rectangular dataset (guess: space separated)")
            theDataset <- readr::read_table(datasetInfo$path2file)
          }
        }
      },
    error=function(e){
      warning("something went wrong while reading dataset, returning NULL")
    })
  return(theDataset)
}

# NOTE: here the "type" 
#' get the type of object (a "dataset")
#' @param x r object
#' @returns list with datasetType one of "vector", "raster", "rasterTS" (rasterBrick), "data.frame"
#' @export
getDatasetObjectTechInfo<-function(x){
  # here we examine the dataset type and the geometry type
  # 
  datasetType <- NULL
  vector_type <- NULL
  if("sf" %in% class(x)){
    datasetType=c(datasetType,"vector")
    vector_type=sf::st_geometry_type(x, by_geometry = FALSE)
  }
  if("RasterBrick" %in% class(x)){
    datasetType=c(datasetType,"rasterTS")
  }
  if("raster" %in% class(x)){
    datasetType=c(datasetType,"raster")
  }
  if("data.frame" %in% class(x)){
    datasetType=c(datasetType,"data.frame")
  }
  res=list(
    datasetType = datasetType,   
    vectorType=vector_type) 
  return(res)
}

plotDatasetGraph<-function(x, type, ...){
  if(type=="raster"){
    x
  }
  if(type=="rasterTS"){
    
  }
  if(type=="shapefile"){
    
  }
  if(type=="rectangular"){}
}

rdsName<-function(name,rdsPath){
  paste0(rdsPath, name, ".RDS")
}

#' save object as RDS file. File name defaults to variable name
#' @param o R object to save
#' @param rdsPath path to destination folder 
#' @param name filename (defaults to variable name)
#' @export
saveObject<-function(o,rdsPath = "~/workspace/", 
                     name = substitute(o)){
  saveRDS(o,rdsName(name, rdsPath))
}


#' write to disk in the user folder both the dataset as RDS file and its 
#' metadata as JSON
#' @param datasetfilename filename to use for the files, without extension
#' @param dataset R object to be saved as RDS
#' @param metadataList a named list containing metadata
#' @param folder path to the destination folder
#' @importFrom stringr str_replace_all
#' @return message a text
#' @export
saveDataset<-function(datasetfilename,dataset,metadataList, folder="~/workspace/"){
  if("/" %in% datasetfilename || "\\" %in% datasetfilename) {
    #warning("The datasetfilename must not include path characters / or \ .")
    return("The datasetfilename must not include path characters / or \ .")
  }
  fname<-stringr::str_replace_all(datasetfilename, " ", "_")
  saveObject(dataset, rdsPath = folder, name=datasetfilename)
  jsonlite::write_json(metadataList,path = paste0(folder,fname,".MD.json"),auto_unbox=T)
  return(paste("Dataset and metadata saved in folder", folder))
}

#' save object as RDS file. File name defaults to variable name
#' @param info metadata info structured in a tibble
#' @param path path to destination folder 
#' @param name filename
#' @noMd
#' @noRd
#' @note IN PROGRESS
saveMetadata<-function(info, path = "~/workspace/", 
                     name = "metadata"){
  warning("TBD")
}

#' save dataset along with its metadata
#' 

