#Meteorological_03_SPEI.R


source(file.path("code", "paths+packages.R"))
data_path<- file.path("data", "Larned_Meteo_Month.CSV")

df_meteo_mo<- readr::read_csv(data_path, col_types = cols())

data_path2<- file.path("data", "ArkLarned_DryPeriods.CSV")
dryPeriods<- readr::read_csv(data_path2, col_types = cols())
rects <- data.frame(start=dryPeriods$date_start, end=dryPeriods$date_end, group=seq_along(start))


df_meteo_mo$date<-as.Date(with(df_meteo_mo, paste(year, month, 1,sep="-")), "%Y-%m-%d")


df_meteo_mo$DOY<-lubridate::yday(df_meteo_mo$date)
###Potential Solar radiation, with latitude in RADIANS.  PotRadiation in [kJ m-2 d-1]
df_meteo_mo$PotRadiation<-EcoHydRology::PotentialSolar(lat=38.205*3.14159265/180, Jday=df_meteo_mo$DOY)

##Calculate PET First
df_meteo_mo$PET<-SPEI::hargreaves(Tmin = df_meteo_mo$tmin_c_mean, Tmax = df_meteo_mo$tmax_c_mean,
                                  Ra = df_meteo_mo$PotRadiation * .001, lat = 38.205, Pre = df_meteo_mo$prcp_mm_sum, na.rm = FALSE )


###Water Balance
CWBAL<-df_meteo_mo$prcp_mm_sum - df_meteo_mo$PET


##Make SPEI classes for 3, 6, 9, 12 month intervals
spei3<-SPEI::spei(df_meteo_mo$prcp_mm_sum - df_meteo_mo$PET, 3)
spei6<-SPEI::spei(CWBAL, 6)
spei9<-SPEI::spei(CWBAL, 9)
spei12<-SPEI::spei(CWBAL, 12)

par(mfrow=c(2,1)) 
plot(spei3) 
plot(spei12, xlab="SPEI")

plot(SPEI::spei(ts(df_meteo_mo$prcp_mm_sum - df_meteo_mo$PET,freq=12,start=c(1998,1)),12))
  plot(rects)
##Plot SPEI
SPEI::plot.spei(spei3, main = "SPEI 3 month interval")

DF3 <- zoo::fortify.zoo(spei3$fitted) %>%
dplyr::select(-Index) %>% 
  dplyr::mutate(Period = zoo::as.yearmon(paste(df_meteo_mo$year, df_meteo_mo$month), "%Y %m")) %>% 
  na.omit() %>% 
  dplyr::mutate(sign = ifelse(ET0_har >= 0, "pos", "neg"))

SPEI3<-ggplot(DF3) +
  geom_bar(aes(x = Period, y = ET0_har, col = sign, fill = sign),
           show.legend = F, stat = "identity") +
  scale_color_manual(values = c("pos" = "darkblue", "neg" = "red")) +
  scale_fill_manual(values = c("pos"  = "darkblue", "neg" = "red")) +
  scale_y_continuous(limits = c(-3, 3), 
                     breaks = -3:3) +
  ylab("SPEI") + ggtitle("3-Month SPEI") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))

DryPeriod<-ggplot()+
    geom_rect(data=rects, inherit.aes=FALSE,
            aes(xmin=start, xmax=end, ymin=min(DF3$ET0_har),
                ymax=max(DF3$ET0_har), group=group), 
            color="transparent", fill="orange", alpha=0.3)+
    lims(x=c(as.Date("1998-10-01"), as.Date("2019-12-31")))
      
Combine3 = SPEI3 / DryPeriod
Combine3


DF6 <- zoo::fortify.zoo(spei6$fitted) %>%
  dplyr::select(-Index) %>% 
  dplyr::mutate(Period = zoo::as.yearmon(paste(df_meteo_mo$year, df_meteo_mo$month), "%Y %m")) %>% 
  na.omit() %>% 
  dplyr::mutate(sign = ifelse(ET0_har >= 0, "pos", "neg"))

SPEI6<-ggplot(DF6) +
  geom_bar(aes(x = Period, y = ET0_har, col = sign, fill = sign),
           show.legend = F, stat = "identity") +
  scale_color_manual(values = c("pos" = "darkblue", "neg" = "red")) +
  scale_fill_manual(values = c("pos"  = "darkblue", "neg" = "red")) +
  scale_y_continuous(limits = c(-3, 3), 
                     breaks = -3:3) +
  ylab("SPEI") + ggtitle("6-Month SPEI") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))
Combine6 = SPEI6 / DryPeriod
Combine6


DF9 <- zoo::fortify.zoo(spei9$fitted) %>%
  dplyr::select(-Index) %>% 
  dplyr::mutate(Period = zoo::as.yearmon(paste(df_meteo_mo$year, df_meteo_mo$month), "%Y %m")) %>% 
  na.omit() %>% 
  dplyr::mutate(sign = ifelse(ET0_har >= 0, "pos", "neg"))

SPEI9<-ggplot(DF9) +
  geom_bar(aes(x = Period, y = ET0_har, col = sign, fill = sign),
           show.legend = F, stat = "identity") +
  scale_color_manual(values = c("pos" = "darkblue", "neg" = "red")) +
  scale_fill_manual(values = c("pos"  = "darkblue", "neg" = "red")) +
  scale_y_continuous(limits = c(-3, 3), 
                     breaks = -3:3) +
  ylab("SPEI") + ggtitle("9-Month SPEI") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))
Combine9 = SPEI9 / DryPeriod
Combine9

DF12 <- zoo::fortify.zoo(spei12$fitted) %>%
  dplyr::select(-Index) %>% 
  dplyr::mutate(Period = zoo::as.yearmon(paste(df_meteo_mo$year, df_meteo_mo$month), "%Y %m")) %>% 
  na.omit() %>% 
  dplyr::mutate(sign = ifelse(ET0_har >= 0, "pos", "neg"))

SPEI12<-ggplot(DF12) +
  geom_bar(aes(x = Period, y = ET0_har, col = sign, fill = sign),
           show.legend = F, stat = "identity") +
  scale_color_manual(values = c("pos" = "darkblue", "neg" = "red")) +
  scale_fill_manual(values = c("pos"  = "darkblue", "neg" = "red")) +
  scale_y_continuous(limits = c(-3, 3), 
                     breaks = -3:3) +
  ylab("SPEI") + ggtitle("12-Month SPEI") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))
Combine12 = SPEI12 / DryPeriod
Combine12

