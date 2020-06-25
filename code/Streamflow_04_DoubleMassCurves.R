## Streamflow_05_DoubleMassCurves.R

## prep workspace
source(file.path("code", "paths+packages.R"))

## Sites to grab
Ark_id <- "07141220" #The Arkansas River at Larned 
Roz_id <- "07141200" # Pawnee River near Rozel
Bur_id <- "07140850" # Pawnee River near Burdett

# start and end date, based on the Ark @ Larned period of record
date_start <- "1998-10-01"
date_end <- "2019-12-31"

##Download data from each site
df_ark <- 
  dataRetrieval::readNWISdv(Ark_id, 
                            parameterCd = "00060",
                            startDate = date_start,
                            endDate = date_end) %>%
  dplyr::select(site_no, Date, X_00060_00003) %>%  
  magrittr::set_colnames(c("gageid", "Date", "discharge_cfs")) %>% 
  dplyr::mutate(discharge_cms = discharge_cfs*(0.3048^3),
                Site = "Ark @ Larned")

df_roz <-
  dataRetrieval::readNWISdv(Roz_id, 
                            parameterCd = "00060",
                            startDate = date_start,
                            endDate = date_end) %>%
  dplyr::select(site_no, Date, X_00060_00003) %>%  
  magrittr::set_colnames(c("gageid", "Date", "discharge_cfs")) %>% 
  dplyr::mutate(discharge_cms = discharge_cfs*(0.3048^3),
                Site = "Pawnee @ Rozel")

df_bur <- 
  dataRetrieval::readNWISdv(Bur_id, 
                            parameterCd = "00060",
                            startDate = date_start,
                            endDate = date_end) %>%
  dplyr::select(site_no, Date, X_00060_00003) %>%  
  magrittr::set_colnames(c("gageid", "Date", "discharge_cfs")) %>% 
  dplyr::mutate(discharge_cms = discharge_cfs*(0.3048^3),
                Site = "Pawnee @ Burdett")

## check for missing data
sum(is.na(df_ark$discharge_cms))
sum(is.na(df_roz$discharge_cms))
sum(is.na(df_bur$discharge_cms))

## calculate cumulative sums
df_ark$Ark_m3_cum <- cumsum(df_ark$discharge_cms*86400)
df_roz$Roz_m3_cum <- cumsum(df_roz$discharge_cms*86400)
df_bur$Bur_m3_cum <- cumsum(df_bur$discharge_cms*86400)

## join
df_all <-
  df_ark %>% 
  dplyr::select(Date, Ark_m3_cum) %>% 
  dplyr::left_join(df_roz[,c("Date", "Roz_m3_cum")], by = "Date") %>% 
  dplyr::left_join(df_bur[,c("Date", "Bur_m3_cum")], by = "Date") %>% 
  dplyr::mutate(YrSinceStart = seq(1, dim(df_all)[1])/365,
                year = lubridate::year(Date),
                DaysSinceReservoir = as.integer(Date - as.Date("2009-09-01")))

## plot
p_ArkVsBur <-
  ggplot(df_all, aes(x = Bur_m3_cum, y = Ark_m3_cum, color = DaysSinceReservoir/365)) +
  geom_point() +
  stat_smooth(data = subset(df_all, Ark_m3_cum > 5e8),
              aes(linetype = DaysSinceReservoir > 0), method = "lm", color = "black") +
  scale_color_gradient2(name = "Years Since\nHorsethief Completed",
                        low = col.cat.red, mid = col.gray, high = col.cat.blu) +
  scale_x_continuous(name = "Pawnee @ Burdett\nCumulative Discharge [m\u00b3]") +
  scale_y_continuous(name = "Arkansas nr Larned\nCumulative Discharge [m\u00b3]") +
  scale_linetype_discrete(name = "Linear\nFit", 
                          labels = c("Before Horsethief", "After Horsethief"))

p_ArkVsRoz <-
  ggplot(df_all, aes(x = Roz_m3_cum, y = Ark_m3_cum, color = DaysSinceReservoir/365)) +
  geom_point() +
  stat_smooth(data = subset(df_all, Ark_m3_cum > 5e8),
              aes(linetype = DaysSinceReservoir > 0), method = "lm", color = "black") +
  scale_color_gradient2(name = "Years Since\nHorsethief Completed",
                        low = col.cat.red, mid = col.gray, high = col.cat.blu) +
  scale_x_continuous(name = "Pawnee @ Rozel\nCumulative Discharge [m\u00b3]") +
  scale_y_continuous(name = "Arkansas nr Larned\nCumulative Discharge [m\u00b3]") +
  scale_linetype_discrete(name = "Linear\nFit", 
                          labels = c("Before Horsethief", "After Horsethief"))

p_BurVsRoz <-
  ggplot(df_all, aes(x = Roz_m3_cum, y = Bur_m3_cum, 
                     color = DaysSinceReservoir/365)) +
  geom_point() +
  stat_smooth(aes(linetype = DaysSinceReservoir > 0), method = "lm", color = "black") +
  scale_color_gradient2(name = "Years Since\nHorsethief Completed",
                        low = col.cat.red, mid = col.gray, high = col.cat.blu) +
  scale_x_continuous(name = "Pawnee @ Rozel\nCumulative Discharge [m\u00b3]") +
  scale_y_continuous(name = "Pawnee @ Burdett\nCumulative Discharge [m\u00b3]") +
  scale_linetype_discrete(name = "Linear\nFit", 
                          labels = c("Before Horsethief", "After Horsethief"))

# save
(p_ArkVsBur + p_ArkVsRoz + p_BurVsRoz + guide_area() +
  plot_layout(ncol = 2, guides = 'collect')) %>% 
  ggsave(file.path("plots", "Streamflow_DoubleMassCurve.png"),
         plot = .,
         width = 190, height = 190, units = "mm")
