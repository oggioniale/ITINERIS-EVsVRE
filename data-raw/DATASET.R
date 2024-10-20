## code to prepare `DATASET` dataset goes here

# execute once when needed
require(readxl)

sites<-readxl::read_xlsx("data-raw/sites_list.xlsx") # SERGI  D:/Projects/ITINERIS/Server/ITINERIS-EVsVRE/
EVs<-readxl::read_xlsx("data-raw/sites_list.xlsx", "EV") # SERGI
datasets<-readxl::read_xlsx("data-raw/sites_list.xlsx", "datasets") # SERGI

neededNames<-c("deimsUUID","ev_id","variablename","datasetname",
               "filename","path2file","type","url","procedure",
               "repo","icon_url")

if(all(neededNames %in% names(datasets))){
  usethis::use_data(sites, EVs, datasets, overwrite = TRUE, internal = TRUE)
} else {
  warning("!!! dataset table source has missing columns. I do not create internal data. Please check.")
}
