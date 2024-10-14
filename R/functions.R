######################################################################################################################
# FUNCTIONS TO DOWNLOAD THE ESSENCIAL VARIABLES (EV)

# This scripts includes the sources and functions to download the data related to the ECV and the EBV variables




######################################################################################################################
######################################################################################################################
######################################################################################################################
######################################################################################################################
# ESSENCIAL CLIMANTE VARIABLES (ECV)

# library(RCurl); library(dplyr); library(data.table); library(tidync); library(magrittr); library(stringr); library(R.utils)

######################################################################################################################
######################################################################################################################
##################                  DWD - GLOBAL PRECIPITATION CLIMATOLOGY CENTRE                   ##################
######################################################################################################################
######################################################################################################################

##### CASES
# https://opendata.dwd.de/climate_environment/GPCC/gpcc_normals_v2022/normals_1951_2000_v2022_025.nc.gz # (func_GPCC) Precipitation 0.25º (varid = precip)
# https://opendata.dwd.de/climate_environment/GPCC/GPCC_DI/ # (dir_GPCC) Drought index (varid = "di_XX")
# https://opendata.dwd.de/climate_environment/GPCC/full_data_monthly_v2022/025/ # All data P, num WS, errors # (dir_GPCC) varid = c("precip", "numgauge", "infilled_numgauges", "interpolation_error", "interpolation_error_infilled", "diff_new_old_method" ) # min. 20 MB - max. 300 MB per gzip archive (10 years per archive) 
# https://opendata.dwd.de/climate_environment/GPCC/GPCC-DI_retro_Analyses/ (dir_GPCC) drought index before 2013 (VARID????) 

# "C:/Users/costy/OneDrive - CNR/Progetti/ITINERIS/Server/Server_v0/Data/DWD/DroughtIndex/GPCC_DI_201301.nc" %>% nc_open() %>% ncvar_get("lon")
# "C:/Users/costy/OneDrive - CNR/Progetti/ITINERIS/Server/Server_v0/Data/DWD/Temperature/Daily/tas_decreg_europe_v20140120_20101201_20101231.nc" %>% nc_open()


##################################################################################
# Function to read a file of GPCC (Global Precipitation Climatology Centre)
#' @description Function to read a file of GPCC (Global Precipitation Climatology Centre)
#' @return 3D matrix
#' @importFrom ncdf4 nc_open ncvar_get
#' @importFrom magrittr %>%
#' @importFrom stringr str_replace
#' @importFrom utils download.file
#' @importFrom R.utils gunzip
#' @param url path of the file to be loaded
#' @param varid variable to be downloaded
#' @export
func_GPCC <- function(url, varid)
{
  # Creating the temporal file for containing the downloaded file
  ncdf_temp <- base::tempfile(pattern = "file_", fileext = paste0("_", basename(url)))
  
  # Downloading the file of the URL and putting it in the ncdf_temp file
  utils::download.file(url = url, destfile = ncdf_temp)
  
  # Decompressing the previous file
  ncdf_temp %>% R.utils::gunzip
  
  # Opening the decompressed NetCDF file
  file_opened <- ncdf4::nc_open(ncdf_temp %>% stringr::str_replace('.gz', ''))
  
  # Adapting the name of the variable because an internal change in the names of the drought index
  varid2 = if(varid != "precip" & any(grepl("^di_", names(file_opened$var)) == F)) {paste0("var", as.numeric(substr(varid, 4,5)))} else {varid}
  
  # Reading the matrix from the NetCDF file file_opened
  ncdf4::ncvar_get(file_opened, varid = varid2)
}





##################################################################################
# How to get the files of a folder of the GPCC (Global Precipitation Climatology Centre)
#' @description Function to get the list of files of GPCC (Global Precipitation Climatology Centre)
#' @return this function returns a list of paths of the selected variable of the GPCC
#' @importFrom RCurl getURL
#' @importFrom magrittr %>%
#' @importFrom XML getHTMLLinks
#' @param dir_GPCC directory of the Global Precipitation Climatology Centre that contain the files of the variables to be downloaded
#' @export
func_files_GPCC <- function(dir_GPCC)
{
  # The URL has firstly the folders of the years and inside them, the subfolders with the files
  # Getting the list of folders of the years within the URL
  cases_gpcc <- dir_GPCC %>%
    RCurl::getURL(verbose = T, ftp.use.epsv = T, dirlistonly = T) %>%
    XML::getHTMLLinks
  
  # Getting the names of the subfolders
  subfolders_gpcc <- cases_gpcc[grep('^(?=.*.../)', cases_gpcc, perl = T)]
  
  # Getting the list of URLs inside the folders and subfolders
  lapply(paste0(dir_GPCC, subfolders_gpcc), 
         function(u) 
         {
           cases2 <- u %>%
             RCurl::getURL(verbose = T, ftp.use.epsv = T, dirlistonly = T) %>%
             XML::getHTMLLinks
           
           paste0(u, cases2[grep('nc.gz', cases2, perl = T)])
         }
         ) %>% 
    unlist
}






##################################################################################
# How to open all files together from a startpoint to an endpoint
#' @description This function reads and merge all files of the selected dir_GPCC
#' @return this function returns a 3D array for the selected variable of the GPCC
#' @importFrom RCurl getURL
#' @importFrom magrittr %>%
#' @importFrom XML getHTMLLinks
#' @param dir_GPCC directory of the Global Precipitation Climatology Centre that contain the files of the variables to be downloaded
#' @param varid variable to be downloaded. This can take the values within c("", "", "")
#' @param start_date initial year of the subset. If NULL, the subset starts in 2013
#' @param end_date final year of the subset. If NULL, the subset ends in the current year
#' @export
array_gpcc <- function(dir_GPCC, varid, start_date = NULL, end_date = NULL)
{
  # Getting the list of URLs inside the directory of the GPCC
  files_gz <- func_files_GPCC(dir_GPCC)
  
  # Selecting the subset of the files from the start and end dates
  case_ini <- if(is.null(start_date)) {1} else {which(str_detect(files_gz, start_date))}
  case_fin <- if(is.null(end_date)) {length(files_gz)} else {which(str_detect(files_gz, end_date))}
  
  selected_gz <- files_gz[case_ini:case_fin]
  
  open_sel_gz = lapply(selected_gz, function(x) func_GPCC(x, varid))
  
  arr1 <- array(0, dim = c(dim(open_sel_gz[[1]])[1], dim(open_sel_gz[[1]])[2], length(open_sel_gz)))
  
  for(i in 1:length(open_sel_gz)){arr1[,,i] <- open_sel_gz[[i]]}
  
  arr1
}





######################################################################################################################
######################################################################################################################
##################                                     WORLDCLIM                                    ##################
######################################################################################################################
######################################################################################################################

##### CASES
# worldclim_global(var, res, path, version="2.1", ...) # TO DOWNLOAD ANY VARIABLE GLOBALLY BASELINE
# worldclim_country(country, var, path, version="2.1", ...) # VARIABLE AT COUNTRY LEVEL BASELINE
# worldclim_tile(var, lon, lat, path, version="2.1", ...) # VARIABLE AT SINGLE TILE BASELINE

# cmip6_world(model, ssp, time, var, res, path, ...) # VARIABLE AT COUNTRY LEVEL FUTURE
# cmip6_tile(lon, lat, model, ssp, time, var, path, ...) # VARIABLE AT SINGLE TILE FUTURE

# FUNCTIONS
library(geodata)


##################################################################################
# BASELINE
# varid %in% c("tmin", "tmax", "tavg", "prec", "srad", "wind", "vapr")
ita <- worldclim_country("Italy", var = "tmin", path = tempdir()) # Country (12 MB) RASTER stack
flor <- worldclim_tile("tmin", 11.255814, 43.769562, path = tempdir()) # LATLON FROM FLORENCE (it gives all central Europe) 66MB raster
world <- worldclim_global("tmin", 5, path = tempdir()) # res %in% 10, 5, 2.5, 0.5 123 MB RASTER STACK


##################################################################################
# FUTURE
# varid %in% c("tmin", "tmax", "tavg", "prec", "bioc")
# model %in% c("ACCESS-CM2", "ACCESS-ESM1-5", "AWI-CM-1-1-MR", "BCC-CSM2-MR", "CanESM5", "CanESM5-CanOE", "CMCC-ESM2", "CNRM-CM6-1", "CNRM-CM6-1-HR", "CNRM-ESM2-1", "EC-Earth3-Veg", "EC-Earth3-Veg-LR", "FIO-ESM-2-0", "GFDL-ESM4", "GISS-E2-1-G", "GISS-E2-1-H", "HadGEM3-GC31-LL", "INM-CM4-8", "INM-CM5-0", "IPSL-CM6A-LR", "MIROC-ES2L", "MIROC6", "MPI-ESM1-2-HR", "MPI-ESM1-2-LR", "MRI-ESM2-0", "UKESM1-0-LL")
# spss %in% c("126", "245", "370", "585")
# time %in% c("2021-2040", "2041-2060", "2061-2080")
fut_flor <- cmip6_tile(11.255814, 43.769562, "IPSL-CM6A-LR", "585", "2021-2040", "tmin", path = tempdir()) # 140 MB RASTER stack
fut_world <- cmip6_world("IPSL-CM6A-LR", "585", "2021-2040", "tmin", 5, path = tempdir()) # 64.5 MB RASTER STACK (RES = 5)





######################################################################################################################
######################################################################################################################
##################         Climate Prediction Center Merged Analysis of Precipitation (CMAP)        ##################
######################################################################################################################
######################################################################################################################

# kyr    ==  year  ( 79 - 99, 00 - 24 )  
# kmn    ==  month (1 - 12)  
# rlat   ==  latitude  (-88.75 --> 88.75)     
# rlon   ==  longitude (eastward from 1.25 E)   
# rain1  ==  monthly precipitation by merging gauge, 5 kinds of satellite estimates (GPI,OPI,SSM/I scattering, SSM/I emission and MSU) and numerical model predictions (mm/day)   
# error1 ==  estimates of relative error for rain1 (%) 
# rain2  ==  monthly precipitation by merging gauge and the 5  kinds of satellite estimates (mm/day) 
# error2 ==  estimates of relative error for rain2 (%) 


##### CASES
# url = "ftp://ftp.cpc.ncep.noaa.gov/precip/cmap/monthly/" (func_files_CMAP) # Precipitation at monthly level 2.5º resolution




# FUNCTIONS
library(RCurl); library(dplyr); library(data.table); library(tidync); library(magrittr); library(stringr); library(R.utils)

# Function to read a file of GPCC (Global Precipitation Climatology Centre)
func_CMAP <- function(url)
{
  file_temp = tempfile(pattern = "file_", fileext = paste0("_", basename(url)))
  
  download.file(url = url, destfile = file_temp)
  
  file_temp %>% gunzip
  data.table::fread(file_temp %>% str_replace('.gz', ''))
  
  # Add the names
}







##################################################################################
# How to get the files of a folder of the Climate Prediction Center Merged Analysis of Precipitation (CMAP)
func_files_CMAP <- function(dir_CMAP)
{
  cases_cmap <- dir_CMAP %>%
    getURL(verbose = T, ftp.use.epsv = T, dirlistonly = T)
  
  cases_cmap2 <- paste(dir_CMAP, strsplit(cases_cmap, "\r*\n")[[1]], sep = "")
  cases_cmap2 <- cases_cmap2[str_detect(cases_cmap2, glob2rx("*v2409_*.txt.gz"))]
  
  years_yy <- sapply(strsplit(cases_cmap2, "_|.txt"), "[", 4) %>% as.numeric
  year_now <- which(years_yy == Sys.Date() %>% format("%y") %>% as.numeric)
  
  cases_cmap2[c((year_now + 1):length(years_yy), 1:year_now)]
}





##################################################################################
# How to open all files together from a startpoint to an endpoint I AM NOT SURE ABOUT THE POINT POSITION BUT I SUGGEST TO DISCARD THIS SOURCE
array_cmap <- function(dir_CMAP, start_year = NULL, end_year = NULL)
{
  files_gz <- func_files_CMAP(dir_CMAP)
  
  case_ini <- if(is.null(start_year)) {1} else {which(str_detect(files_gz, substr(start_year, nchar(start_year) - 1, nchar(start_year))))}
  case_fin <- if(is.null(end_year)) {length(files_gz)} else {which(str_detect(files_gz, substr(end_year, nchar(end_year) - 1, nchar(end_year))))}
  
  selected_gz <- files_gz[case_ini:case_fin]
  
  open_sel_gz = lapply(selected_gz, function(x) func_CMAP(x))
  
  cases_x = unique(open_sel_gz[[1]]$V3); cases_y = unique(open_sel_gz[[1]]$V4); cases_z = unique(open_sel_gz[[1]]$V2)
  
  arr1 <- array(0, dim = c(length(cases_x), length(cases_y), length(cases_z) * length(open_sel_gz)))
  
  count = 1
  
  for(i in 1:length(open_sel_gz))
  {
    for (j in 1:length(cases_z))
    {
      subs_case = open_sel_gz[[i]] %>% filter(V2 == j)
      
      arr1[,,i] = array(subs_case[,5] %>% unlist, c(length(cases_x), length(cases_y)))
      
      count = count + 1
    }
  }
  
  arr1
}






######################################################################################################################
######################################################################################################################
##################                                ISD-NCEI-NOAA                                     ##################
######################################################################################################################
######################################################################################################################

# CASES

############# DOES NOT WORK
library(HelpersMG)
wget_string="wget -N -nH -nd -r -e robots=off --no-parent --force-html -A.nc https://data.nodc.noaa.gov/woa/WOA18/DATA/temperature/netcdf/5564/"
s = system(wget_string, intern = TRUE)

download.file('https://data.nodc.noaa.gov/woa/WOA18/DATA/temperature/netcdf/5564/', 
              destfile = "C:/Users/costy/", 
              method = "auto", 
              extra = "-N -nH -nd -r -e robots=off --no-parent --force-html -A.nc")

wget(wget_string)
install.packages("httr")
library(httr)

netrc_path <- "/path/to/.netrc"
cookie_path <- "/path/to/.urs_cookies"
downloaded_file_path <- "/path/to/filename"
# Before using the script
#Set up your ~/.netrc file as listed here: https://wiki.earthdata.nasa.gov/display/EL/How+To+Access+Data+With+cURL+And+Wget
set_config(config(followlocation=1,netrc=1,netrc_file=netrc_path,cookie=cookie_path,cookiefile=cookie_path,cookiejar=cookie_path))
sss = httr::GET(url = "https://disc2.gesdisc.eosdis.nasa.gov/data/TRMM_RT/TRMM_3B42RT_Daily.7/2000/03/3B42RT_Daily.20000301.7.nc4",
          write_disk(tempfile(), overwrite = TRUE))

download.file('http://cran-logs.rstudio.com/2019/2019-10-13.csv.gz', tempfile(), method = "auto")
plot(sss)




######################################################################################################################
######################################################################################################################
##################                 Interface to 'ECMWF' and 'CDS' Data Web Services                 ##################
######################################################################################################################
#########################################################################################################3#############
################### DOES NOT WORK
# Installing and reading the library
install.packages("ecmwfr")
library(ecmwfr)

# My own credentials for the ECMWF
wf_set_key("b01f8d62fb3477dc7298dfd41236db14", user = "sergi.costafredaaumedes@cnr.it")
wf_get_key(user = "sergi.costafredaaumedes@cnr.it")


ERA = wf_archetype(
  request = list(
    dataset_short_name = "reanalysis-era5-pressure-levels",
    product_type = "reanalysis",
    variable = "geopotential",
    year = "2024",
    month = "03",
    day = "01",
    time = "13:00",
    pressure_level = "1000",
    data_format = "grib",
    target = "download.grib"
  ),
  dynamic_fields = c("year", "day", "target")
)

str(ERA(2021, 3, "new_download.grip"))


request <- list(
  dataset_short_name = "reanalysis-era5-pressure-levels",
  product_type = "reanalysis",
  variable = "geopotential",
  year = "2024",
  month = "03",
  day = "01",
  time = "13:00",
  pressure_level = "1000",
  data_format = "grib",
  target = "download.grib"
)

wf_request(request = request)

wf_datasets()
wf_check_request(request)


#########################################################################################################
### ESSENCIAL BIODIVERSITY VARIABLES (EBV)



######################################################################################################################
######################################################################################################################
##################                                      PEP725                                      ##################
######################################################################################################################
######################################################################################################################


# How to install the PHENOR package
remotes::install_github("bluegreen-labs/phenor@v1.3.1")

library(phenor); library(ecmwfr)

################################# PHENOLOGY
# The list of the varieties and their ID_species
list_species = check_pep725_species(list = T)

# How to get the phenology for an specific variety (110 is Picea abies)
picea_obs = pr_dl_pep725("C:/Users/costy/OneDrive/Documentos/Credentials.txt", 
             species = 110, path = tempdir(), internal = T)

# How to get the ID of the species
quercus <- phenor::check_pep725_species(species = "quercus")

################################# TEMPERATURE
# Additionally, this gives the Berkeley Earth Gridded mean daily temperature data
pr_dl_be(path = "C:/Users/costy/Dropbox/Clases", year = 2022) # removing year = XXXX, downloads all the data

# My own credentials for the COPERNICUS CDS
wf_set_key("a8183d4b-2fe8-4ccc-905b-a38aaca9740a", user = "311229")
wf_get_key(user = "311229")

# Download Global CMIP6 driver data
pr_dl_cmip_sca(
  path = tempdir(),
  end_year = 2100,
  model = "miroc6",
  scenario = "ssp5_8_5",
  variable = "precipitation", # c("daily_maximum_near_surface_air_temperature", "daily_minimum_near_surface_air_temperature", "precipitation")
  extent = c(40, -80, 50, -70),
  user = "311229:a8183d4b-2fe8-4ccc-905b-a38aaca9740a"
)


library(ecmwfr)

cds.key <- "a8183d4b-2fe8-4ccc-905b-a38aaca9740a"; user_cds = "311229"
wf_set_key(user = user_cds, key = cds.key)

request <- list(
  dataset_short_name = "reanalysis-era5-pressure-levels",
  product_type = "reanalysis",
  format = "netcdf",
  variable = "temperature",
  pressure_level = "850",
  year = "2016",
  month = "08",
  day = "16",
  time = c("00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00", "08:00", "09:00", "10:00", "11:00", "12:00", "13:00", "14:00", "15:00", "16:00", "17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00"),
  # area is specified as N, W, S, E
  area = c(50, -20, 30, 20),
  target = "download_e5_pressure.nc"
)

file <- wf_request(user = user_cds,
                   request = request,
                   transfer = TRUE,
                   path = "~",
                   verbose = TRUE)

#### COPERNICUS NON MI PERMETTE ACCEDERE CON LE MIE CREDENZIALI

# phenor::pr_dl_be		Download Berkeley Earth Gridded mean daily temperature data
# phenor::pr_dl_cmip		Download Global CMIP6 driver data
# phenor::pr_dl_era5		Download Global ERA5 (land) driver data
# phenor::pr_dl_npn		Download USA-NPN data using the API interface
# phenor::pr_dl_pep725		Download PEP725 data





######################################################################################################################
######################################################################################################################
##################          Interface to the Global Biodiversity Information Facility API           ##################
######################################################################################################################
######################################################################################################################

# Libraries
install.packages("rgbif")
library(rgbif); library(sp); library(rnaturalearth)

# List of tree species
tree_file <- "https://gist.githubusercontent.com/jhnwllr/bd61bcd56d76beeacd03ea9ace0a31fd/raw/089d4c3a88b358719845a1394c9f88f9a2025e20/tree_names.tsv"
long_checklist <- readr::read_tsv(tree_file)

# Set the species
Species.Name <- "pinus canariensis" # to be selected by the user

#Search GBIF and download observations
Species.key <- name_backbone(name=Species.Name)$speciesKey
locations <- occ_search(taxonKey=Species.key,hasCoordinate = TRUE, return='data', limit=20000)

#Remove localities with uncertain coordinates - meaning the observer entered approximate latitude and longitude data. 
# This is common with very old specimens/records that were collected prior to handheld GPS devices being invented
locations2 <- subset(locations$data, coordinateUncertaintyInMeters <= 5000)

# Spatialize the observations
loc_pts <- SpatialPoints(coords = locations2[c("decimalLongitude","decimalLatitude")],
              proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "))

#### Plotting the observations

# Reading the world map
world <- ne_countries(scale = "medium", returnclass = "sf")

# plot both spatial objects
plot(world$geometry, xlim = c(-1, 12), ylim = c(30, 45))
plot(loc_pts,add=T,col="red")






######################################################################################################################
######################################################################################################################
##################                                    PHENOCAMR                                     ##################
######################################################################################################################
######################################################################################################################

# Install and read the library PhenocamR
install.packages("phenocamr")
library(phenocamr)

# veg_type %in% c(
# "AG" - Agriculture
# "DB" - Deciduous Broadleaf
# "EB" - Evergreen Broadleaf
# "DN" - Deciduous Needleaf
# "EN" - Evergreen Needleleaf
# "GR" - Grassland
# "NV" - Non-vegetated
# "RF" - Reference Panel
# "SH" - Shrub
# "UN" - Understory
# "WL" - Wetland
# "XX" - Mixed/Canopy/Other
# )


# Download the requested phenocam data (data downloaded in the temporary folder)
download_phenocam(site = "harvard$",
                  veg_type = "DB",
                  roi_id = "1000",
                  frequency = "3")
df <- read_phenocam(file.path(tempdir(),"harvard_DB_1000_3day.csv"))

# Read the available ROIS and the sites with their timeseries
df <- list_rois() # The data available is found here for the GCC timeseries: https://phenocam.nau.edu/webcam/roi/search/
df <- list_sites() # The data available is found here for the camera NDVI timeseries: https://phenocam.nau.edu/webcam/roi/ndvi/

# Merging the weatehr and the phenocam data
merged_db <- merge_daymet(file.path(tempdir(),"harvard_DB_1000_3day.csv"))
merged_db$data # This DB contains only the data required. The other has additional information such as the metadata







######################################################################################################################
######################################################################################################################
##################         EUROPEAN ENVIRONMENTAL AGENCY - database-habitats-directive 2020         ##################
######################################################################################################################
######################################################################################################################

# DATA PREVIOUSLY DOWNLOADED!!!!!!!!!!!!!!!!!!!!!

aaa ="C:/Users/costy/Downloads/GLODAPv2.2023_Merged_Master_File/GLODAPv2.2023_Merged_Master_File.csv" %>%
  fread %>% select(G2latitude, G2longitude)
