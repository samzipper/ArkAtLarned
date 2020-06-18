## Streamflow_01_DownloadData.R

## prep workspace
source(file.path("code", "paths+packages.R"))

##Grab data from Arkansas River
USGS_id <- "07141220" #The Arkansas River at Larned 

df_site <- dataRetrieval::readNWISdv(USGS_id, "00060",
                                     "","") %>%
  dplyr::select(site_no, Date, X_00060_00003) %>% 
  magrittr::set_colnames(c("gageid", "Date", "discharge_cfs")) %>% 
  dplyr::mutate(discharge_cms = discharge_cfs*(0.3048^3),
                year = lubridate::year(Date),
                month = lubridate::month(Date))
