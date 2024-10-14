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
           # Getting the list of files and their characteristics within the subfolders
           cases2 <- u %>%
             RCurl::getURL(verbose = T, ftp.use.epsv = T, dirlistonly = T) %>%
             XML::getHTMLLinks
           
           # Getting only the paths
           paste0(u, cases2[grep('nc.gz', cases2, perl = T)])
         }
         ) %>% 
    
    # Unlisting the previous list
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
  
  # Selecting the files considering the start and end dates
  selected_gz <- files_gz[case_ini:case_fin]
  
  # Opening all selected files as a list of 3D arrays
  open_sel_gz = lapply(selected_gz, function(x) func_GPCC(x, varid))
  
  # Creating an empty 3D array with the dimensions of the arrays of the previous list
  arr1 <- array(0, dim = c(dim(open_sel_gz[[1]])[1], dim(open_sel_gz[[1]])[2], length(open_sel_gz)))
  
  # Adding the arrays of the list in the larger one
  for(i in 1:length(open_sel_gz)){arr1[,,i] <- open_sel_gz[[i]]}
  
  # Showing the final array
  arr1
}
