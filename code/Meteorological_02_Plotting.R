#Meteorological_02_Plotting.R

source(file.path("code", "paths+packages.R"))

data_path<- file.path("data", "Larned_Meteo.CSV")

df_meteo<- readr::read_csv(data_path, col_types = cols())
# set units


df_meteo$prcp <- NULL
df_meteo$tmax <- NULL
df_meteo$tmin <- NULL

# summarize by year, month
df_meteo$year <- lubridate::year(df_meteo$date)
df_meteo$month <- lubridate::month(df_meteo$date)



df_meteo_mo <-
  df_meteo %>% 
  dplyr::group_by(year, month) %>% 
  dplyr::summarize(n_prcp = sum(is.finite(prcp_mm)),
                   n_tmax = sum(is.finite(tmax_c)),
                   n_tmin = sum(is.finite(tmin_c)),
                   prcp_mm_sum = sum(prcp_mm, na.rm = T),
                   tmax_c_mean = mean(tmax_c, na.rm = T),
                   tmin_c_mean = mean(tmin_c, na.rm = T)) %>% 
  dplyr::mutate(n_days_mo = lubridate::days_in_month(month)) %>% 
  dplyr::ungroup()
readr::write_csv(df_meteo_mo, file.path("data", "LArned_Meteo_Month.CSV"))


#Assign Date Value for middle of month to plot summary stats
df_meteo_mo$date <- paste(df_meteo_mo$year, df_meteo_mo$month, "15", sep="-") %>% ymd() %>% as.Date()

# set months with too many missing days to NA
day_thres <- 3
df_meteo_mo$prcp_mm_sum[(df_meteo_mo$n_days_mo - df_meteo_mo$n_prcp > day_thres)] <- NA
df_meteo_mo$tmax_c_mean[(df_meteo_mo$n_days_mo - df_meteo_mo$n_tmax > day_thres)] <- NA
df_meteo_mo$tmin_c_mean[(df_meteo_mo$n_days_mo - df_meteo_mo$n_tmin > day_thres)] <- NA

## annual totals
df_meteo_yr <- 
  df_meteo_mo %>% 
  dplyr::group_by(year) %>% 
  dplyr::summarize(prcp_mm = sum(prcp_mm_sum),
                   tmax_c = mean(tmax_c_mean),
                   tmin_c = mean(tmin_c_mean)) %>% 
  dplyr::ungroup()
df_meteo_yr$date <- paste(df_meteo_yr$year, "6", "15", sep="-") %>% ymd() %>% as.Date()


## annual extremes
df_meteo_yr_extreme <-
  df_meteo %>% 
  dplyr::group_by(year) %>% 
  dplyr::summarize(prcp_gt_25mm = sum(prcp_mm > 25),
                   prcp_gt_50mm = sum(prcp_mm > 50),
                   prcp_gt_75mm = sum(prcp_mm > 75))

ggplot(data=df_meteo)+
  geom_line(aes(x=date, y= tmax_c, color = 'Max Temp (C)'))+
  geom_line(aes(x=date, y= tmin_c, color = 'Min Temp (C)'))+
  geom_line(aes(x=date, y= prcp_mm, color = 'Precip (mm)'))+
  scale_y_continuous(
    name = "Temp C",
    sec.axis = sec_axis( trans=~.*1, name="Precip (mm)")
    )



#########Read in  Hydrograph Data############
path_hydro <- file.path("data", "ArkLarned_Baseflow.CSV")
df_hydro<- readr::read_csv(path_hydro, col_types = cols())


river<- df_hydro %>%
ggplot() +
  geom_line(aes(x=Date, y= log10(discharge_cms+.01)))+
  geom_line(aes(x=Date, y = log10(bt +.01)), color = col.cat.blu)+
  geom_rect(data=rects, inherit.aes=FALSE,
            aes(xmin=start, xmax=end, ymin= min(log10(df_hydro$discharge_cms)),
            ymax= max(log10(df_hydro$discharge_cms)), group=group), 
            color="transparent", fill="orange", alpha=0.3)+
  labs(x = "Date",
       y = "Log10 Discharge (cms)")

 rain<-ggplot()+
   geom_line(data=df_meteo_mo, aes(x=date, y= prcp_mm_sum))+
   geom_rect(data=rects, inherit.aes=FALSE,
             aes(xmin=start, xmax=end, ymin= min(df_meteo_mo$prcp_mm_sum),
                 ymax= max(df_meteo_mo$prcp_mm_sum), group=group), 
             color="transparent", fill="orange", alpha=0.3)+
   labs(x = "Date",
        y = "monthly cumulative Precipitation")

p <-river / rain
p
## plots
# monthly trends
ggplot(df_meteo_mo, aes(x = year, y = prcp_mm_sum)) +
  geom_line() +
  facet_wrap(~month, scales = "free_y") +
  stat_smooth(method = "lm")

ggplot(df_meteo_mo, aes(x = year, y = tmax_c_mean)) +
  geom_point() +
  facet_wrap(~month, scales = "free_y") +
  stat_smooth(method = "lm")

ggplot(df_meteo_mo, aes(x = year, y = tmin_c_mean)) +
  geom_point() +
  facet_wrap(~month, scales = "free_y") +
  stat_smooth(method = "lm")

# annual trends
ggplot(df_meteo_yr, aes(x = year, y = prcp_mm)) +
  geom_point() +
  stat_smooth(method = "lm")

ggplot(df_meteo_yr, aes(x = year, y = tmax_c)) +
  geom_point() +
  stat_smooth(method = "lm")

ggplot(df_meteo_yr, aes(x = year, y = tmin_c)) +
  geom_point() +
  stat_smooth(method = "lm")

