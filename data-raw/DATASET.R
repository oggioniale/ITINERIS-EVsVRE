## code to prepare `DATASET` dataset goes here

# execute once when needed
require(readxl)

sites<-readxl::read_xlsx("static_data/sites_list.xlsx")
EVs<-readxl::read_xlsx("static_data/sites_list.xlsx", "EV")
datasets<-readxl::read_xlsx("static_data/sites_list.xlsx", "datasets")

usethis::use_data(sites, EVs, datasets, overwrite = TRUE, internal = TRUE)

