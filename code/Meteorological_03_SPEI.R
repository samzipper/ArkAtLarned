#Meteorological_03_SPEI.R


source(file.path("code", "paths+packages.R"))
data_path<- file.path("data", "Larned_Meteo_Month.CSV")

df_meteo_mo<- readr::read_csv(data_path, col_types = cols())

##Calculate PET First
df_meteo_mo$PET<-SPEI::hargreaves(Tmin = df_meteo_mo$tmin_c_mean, Tmax = df_meteo_mo$tmax_c_mean, lat = 38.205 )


###Water Balance
CWBAL<-df_meteo_mo$prcp_mm_sum - df_meteo_mo$PET


##Make SPEI classes for 3, 6, 9, 12 month intervals
spei3<-SPEI::spei(CWBAL, 3)
spei6<-SPEI::spei(CWBAL, 6)
spei9<-SPEI::spei(CWBAL, 9)
spei12<-SPEI::spei(CWBAL, 12)

##Plot SPEI
SPEI::plot.spei(spei3, main = "SPEI 3 month interval")
  
SPEI::plot.spei(spei6, main = "SPEI 6 month interval")
SPEI::plot.spei(spei9, main = "SPEI 9 month interval")
SPEI::plot.spei(spei12, main = "SPEI 12 month interval")
