#Meteo Data

library(tidyverse)
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

# inspect data continuity
rnoaa::vis_miss(df)

# set units
df$prcp_mm <- df$prcp/10
df$tmax_c <- df$tmax/10
df$tmin_c <- df$tmin/10

df$prcp <- NULL
df$tmax <- NULL
df$tmin <- NULL

# summarize by year, month
df$year <- lubridate::year(df$date)
df$month <- lubridate::month(df$date)

df_mo <-
  df %>% 
  dplyr::group_by(year, month) %>% 
  dplyr::summarize(n_prcp = sum(is.finite(prcp_mm)),
                   n_tmax = sum(is.finite(tmax_c)),
                   n_tmin = sum(is.finite(tmin_c)),
                   prcp_mm_sum = sum(prcp_mm, na.rm = T),
                   tmax_c_mean = mean(tmax_c, na.rm = T),
                   tmin_c_mean = mean(tmin_c, na.rm = T)) %>% 
  dplyr::mutate(n_days_mo = lubridate::days_in_month(month)) %>% 
  dplyr::ungroup()

# set months with too many missing days to NA
day_thres <- 3
df_mo$prcp_mm_sum[(df_mo$n_days_mo - df_mo$n_prcp > day_thres)] <- NA
df_mo$tmax_c_mean[(df_mo$n_days_mo - df_mo$n_tmax > day_thres)] <- NA
df_mo$tmin_c_mean[(df_mo$n_days_mo - df_mo$n_tmin > day_thres)] <- NA

## annual totals
df_yr <- 
  df_mo %>% 
  dplyr::group_by(year) %>% 
  dplyr::summarize(prcp_mm = sum(prcp_mm_sum),
                   tmax_c = mean(tmax_c_mean),
                   tmin_c = mean(tmin_c_mean)) %>% 
  dplyr::ungroup()

## annual extremes
df_yr_extreme <-
  df %>% 
  dplyr::group_by(year) %>% 
  dplyr::summarize(prcp_gt_25mm = sum(prcp_mm > 25),
                   prcp_gt_50mm = sum(prcp_mm > 50),
                   prcp_gt_75mm = sum(prcp_mm > 75))

## plots
# monthly trends
ggplot(df_mo, aes(x = year, y = prcp_mm_sum)) +
  geom_point() +
  facet_wrap(~month, scales = "free_y") +
  stat_smooth(method = "lm")

ggplot(df_mo, aes(x = year, y = tmax_c_mean)) +
  geom_point() +
  facet_wrap(~month, scales = "free_y") +
  stat_smooth(method = "lm")

ggplot(df_mo, aes(x = year, y = tmin_c_mean)) +
  geom_point() +
  facet_wrap(~month, scales = "free_y") +
  stat_smooth(method = "lm")

# annual trends
ggplot(df_yr, aes(x = year, y = prcp_mm)) +
  geom_point() +
  stat_smooth(method = "lm")

ggplot(df_yr, aes(x = year, y = tmax_c)) +
  geom_point() +
  stat_smooth(method = "lm")

ggplot(df_yr, aes(x = year, y = tmin_c)) +
  geom_point() +
  stat_smooth(method = "lm")

