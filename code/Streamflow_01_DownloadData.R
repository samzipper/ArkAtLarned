## Streamflow_01_DownloadData.R

## prep workspace
source(file.path("code", "paths+packages.R"))

##Grab data from Arkansas River
USGS_id <- "07141220" #The Arkansas River at Larned 


##This has issues in the code.  Not recognizing dplyr 
##Download data from USGS Gage
df_site <- dataRetrieval::readNWISdv(USGS_id, "00060","","") %>%
  dplyr::select(site_no, Date, X_00060_00003) %>%  
  magrittr::set_colnames(c("gageid", "Date", "discharge_cfs")) %>% 
  dplyr::mutate(discharge_cms = discharge_cfs*(0.3048^3),
                year = lubridate::year(Date),
                month = lubridate::month(Date))

#Export the Data to a CSV in the Data folder
readr::write_csv(df_site, file.path("data", "Historical_Larned_ArkFlow.CSV"))

#Processess
#1, Download Datata from USGS and Transform into recognizable format
#2. Export as a spread sheet to the 'data' folder



