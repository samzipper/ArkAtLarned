#Meteo Data

source(file.path("code", "paths+packages.R"))
library(rnoaa)


## get data
#Larned, Larned, Sanford, Great Bend
staid_larned1 <- "USC00144530"
df_larned1 <- 
  rnoaa::meteo_tidy_ghcnd(staid_larned1, var = c("PRCP", "TMAX", "TMIN"), date_min = '1998-01-01', date_max = '2019-12-31') %>% 
  dplyr::mutate(station_prcp = staid_larned1)

# Larned2
staid_larned2 <- "USC00144531"
df_larned2 <- 
  rnoaa::meteo_tidy_ghcnd(staid_larned2, var = c("PRCP", "TMAX", "TMIN"), date_min = 1998-01-01)

# Sanford
staid_sanford<- "USC00147192"
df_sanford <- 
  rnoaa::meteo_tidy_ghcnd(staid_sanford, var = c("PRCP", "TMAX", "TMIN"), date_min = 1998-01-01)

# GreatBend ENE 2.1
staid_greatBend <- "USC00143218"
df_greatBend <-
  rnoaa::meteo_tidy_ghcnd(staid_greatBend, var = c("PRCP", "TMAX", "TMIN"), date_min = 1998-01-01)

## combine
date<-  seq.Date(from =as.Date("1998-01-01"), to =as.Date("2019-12-31"), by = "day")
df <-as.data.frame(date)
df = left_join(df, df_larned1, by="date")

# fill in missing precip
prcp_missing_dates <- df$date[is.na(df$prcp)]
for (d in prcp_missing_dates){
  if (sum(is.finite(df_larned2$prcp[df_larned2$date==d])) > 0){
    df$prcp[df$date==d] <- df_larned2$prcp[df_larned2$date==d]
    df$station_prcp[df$date==d] <- staid_larned2
    
  } else if (sum(is.finite(df_sanford$prcp[df_sanford$date==d])) > 0){
    df$prcp[df$date==d] <- df_sanford$prcp[df_sanford$date==d]
    df$station_prcp[df$date==d] <- staid_sanford
  
    
  } else if (sum(is.finite(df_greatBend$prcp[df_greatBend$date==d])) > 0){
    df$prcp[df$date==d] <- df_greatBend$prcp[df_greatBend$date==d]
    df$station_prcp[df$date==d] <- staid_greatBend
    
  } else {
    df$station_prcp[df$date==d] <- "No Data"
  }
}

tmax_missing_dates <- df$date[is.na(df$tmax)]
for (d in tmax_missing_dates){
  if (sum(is.finite(df_larned2$tmax[df_larned2$date==d])) > 0){
    df$tmax[df$date==d] <- df_larned2$tmax[df_larned2$date==d]
    df$station_tmax[df$date==d] <- staid_larned2
    
  } else if (sum(is.finite(df_sanford$tmax[df_sanford$date==d])) > 0){
    df$tmax[df$date==d] <- df_sanford$tmax[df_sanford$date==d]
    df$station_tmax[df$date==d] <- staid_sanford
    
    
  } else if (sum(is.finite(df_greatBend$tmax[df_greatBend$date==d])) > 0){
    df$tmax[df$date==d] <- df_greatBend$tmax[df_greatBend$date==d]
    df$station_tmax[df$date==d] <- staid_greatBend
    
  } else {
    df$station_tmax[df$date==d] <- "No Data"
  }
}

tmin_missing_dates <- df$date[is.na(df$tmin)]
for (d in tmin_missing_dates){
  if (sum(is.finite(df_larned2$tmin[df_larned2$date==d])) > 0){
    df$tmin[df$date==d] <- df_larned2$tmin[df_larned2$date==d]
    df$station_tmin[df$date==d] <- staid_larned2
    
  } else if (sum(is.finite(df_sanford$tmin[df_sanford$date==d])) > 0){
    df$tmin[df$date==d] <- df_sanford$tmin[df_sanford$date==d]
    df$station_tmin[df$date==d] <- staid_sanford
  
    
  } else if (sum(is.finite(df_greatBend$tmin[df_greatBend$date==d])) > 0){
    df$tmin[df$date==d] <- df_greatBend$tmin[df_greatBend$date==d]
    df$station_tmin[df$date==d] <- staid_greatBend
    
  } else {
    df$station_tmin[df$date==d] <- "No Data"
  }
}

df$prcp_mm <- df_meteo$prcp/10
df$tmax_c <- df_meteo$tmax/10
df$tmin_c <- df_meteo$tmin/10
# inspect data continuity
rnoaa::vis_miss(df)

##Write to Data

readr::write_csv(df, file.path("data", "Larned_Meteo.CSV"))

