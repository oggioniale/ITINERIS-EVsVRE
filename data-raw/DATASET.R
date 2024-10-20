## code to prepare `DATASET` dataset goes here

# execute once when needed
require(readxl)
#xlsxFile<-"data-raw/sites_list_new.xlsx"
xlsxFile<-"data-raw/sites_list.xlsx"

sites<-readxl::read_xlsx(xlsxFile) # SERGI  D:/Projects/ITINERIS/Server/ITINERIS-EVsVRE/
EVs<-readxl::read_xlsx(xlsxFile, "EV") # SERGI
datasets<-readxl::read_xlsx(xlsxFile, "datasets") # SERGI


#cat("c(",paste0("\"",names(EVs),"\"",collapse=","),")")
#cat("c(",paste0("\"",names(sites),"\"",collapse=","),")")
#cat("c(",paste0("\"",names(datasets),"\"",collapse=","),")")
neededNamesSites<-c( "id","deimsUUID","name","domain","active","url","alt_name" )

neededNamesDatasets<-c("deimsUUID","ev_id","variablename","datasetname",
                       "filename","path2file","type","url","procedure",
                       "repo","icon_url")

neededNamesEVs<-c( "id","name","type","domain","description","webpage","uom" )


all_needed_names_ok<-all(
  all(neededNamesSites %in% names(sites)),
  all(neededNamesEVs %in% names(EVs)),
  all(neededNamesDatasets %in% names(datasets))
)
if(all_needed_names_ok){
  usethis::use_data(sites, EVs, datasets, overwrite = TRUE, internal = TRUE)
  #clean up
  rm(sites, EVs, datasets, 
     all_needed_names_ok,neededNamesSites, neededNamesDatasets, neededNamesEVs,
     xlsxFile)
} else {
  warning("!!! table source has missing columns. I do not create internal data. Please check.")
}



