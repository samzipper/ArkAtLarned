#Meteorological_03_SPEI.R


source(file.path("code", "paths+packages.R"))
data_path<- file.path("data", "Larned_Meteo_Month.CSV")

df_meteo_mo<- readr::read_csv(data_path, col_types = cols())

data_path2<- file.path("data", "ArkLarned_DryPeriods.CSV")
dryPeriods<- readr::read_csv(data_path2, col_types = cols())
rects <- data.frame(start=dryPeriods$date_start, end=dryPeriods$date_end, group=seq_along(start))


data_path3<- file.path("data", "MonthlyStreamflow.CSV")
monthly_Ark<-readr::read_csv(data_path3, col_types = cols())


df_meteo_mo$date<-as.Date(with(df_meteo_mo, paste(year, month, 1,sep="-")), "%Y-%m-%d")


df_meteo_mo$DOY<-lubridate::yday(df_meteo_mo$date)
###Potential Solar radiation, with latitude in RADIANS.  PotRadiation in [kJ m-2 d-1]
df_meteo_mo$PotRadiation<-EcoHydRology::PotentialSolar(lat=38.205*3.14159265/180, Jday=df_meteo_mo$DOY)

##Calculate PET First
df_meteo_mo$PET<-SPEI::hargreaves(Tmin = df_meteo_mo$tmin_c_mean, Tmax = df_meteo_mo$tmax_c_mean,
                                  Ra = df_meteo_mo$PotRadiation * .001, 
                                  lat = 38.205, Pre = df_meteo_mo$prcp_mm_sum, 
                                  na.rm = FALSE )


###Water Balance
CWBAL<-df_meteo_mo$prcp_mm_sum - df_meteo_mo$PET
df_meteo_mo$CWBAL<-CWBAL

spei_inputs<-ggplot(data=df_meteo_mo,)+
  geom_line(aes(x=date, y=PET, colour="PET"))+
  geom_line(aes(x=date, y = prcp_mm_sum, colour="Precipitation"))+
  geom_line(aes(x=date, y = CWBAL, colour="Water Balance"))+
  geom_hline(yintercept = 0, color = "gray65")+
   labs(y="Water Amount (mm)",
       x= NULL,
       colour="Legend")+
  theme(plot.title = element_text(hjust = 0.5))

spei_inputs
ggsave(file.path('plots', "SPEI_Inputs.PNG"),
       width = 8, height = 4, units = "in")


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
  dplyr::mutate(Period = as.Date(paste(df_meteo_mo$year, df_meteo_mo$month, "1", sep="-"))) %>% 
  na.omit() %>% 
  dplyr::mutate(sign = ifelse(ET0_har >= 0, "pos", "neg"))

SPEI3<-ggplot(DF3) +
  geom_rect(data=rects, inherit.aes=FALSE,
            aes(xmin=start, xmax=end, ymin=min(DF3$ET0_har),
                ymax=max(DF3$ET0_har), group=group), 
            color="transparent", fill="orange", alpha=0.3)+
  geom_bar(aes(x = Period, y = ET0_har, col = sign, fill = sign),
           show.legend = F, stat = "identity") +
  scale_color_manual(values = c("pos" = "darkblue", "neg" = "red")) +
  scale_fill_manual(values = c("pos"  = "darkblue", "neg" = "red")) +
  ylab("3-month SPEI") +
  xlab(NULL)+
  theme(plot.title = element_text(hjust = 0.5))+ 
  lims(x=c(as.Date("1998-10-01"), as.Date("2019-12-31")))

SPEI3



DF6 <- zoo::fortify.zoo(spei6$fitted) %>%
  dplyr::select(-Index) %>% 
  dplyr::mutate(Period = as.Date(paste(df_meteo_mo$year, df_meteo_mo$month, "1", sep="-"))) %>% 
  na.omit() %>% 
  dplyr::mutate(sign = ifelse(ET0_har >= 0, "pos", "neg"))

SPEI6<-ggplot(DF6) +
  geom_rect(data=rects, inherit.aes=FALSE,
            aes(xmin=start, xmax=end, ymin=min(DF6$ET0_har),
                ymax=max(DF6$ET0_har), group=group), 
            color="transparent", fill="orange", alpha=0.3)+
  geom_bar(aes(x = Period, y = ET0_har, col = sign, fill = sign),
           show.legend = F, stat = "identity") +
  scale_color_manual(values = c("pos" = "darkblue", "neg" = "red")) +
  scale_fill_manual(values = c("pos"  = "darkblue", "neg" = "red")) +
  ylab("SPEI") + ggtitle("6-Month SPEI") +
  theme(plot.title = element_text(hjust = 0.5))+ 
  lims(x=c(as.Date("1998-10-01"), as.Date("2019-12-31")))

SPEI6

DF9 <- zoo::fortify.zoo(spei9$fitted) %>%
  dplyr::select(-Index) %>% 
  dplyr::mutate(Period = as.Date(paste(df_meteo_mo$year, df_meteo_mo$month, "1", sep="-"))) %>% 
  na.omit() %>% 
  dplyr::mutate(sign = ifelse(ET0_har >= 0, "pos", "neg"))

SPEI9<-ggplot(DF9) +
  geom_rect(data=rects, inherit.aes=FALSE,
            aes(xmin=start, xmax=end, ymin=min(DF6$ET0_har),
                ymax=max(DF6$ET0_har), group=group), 
            color="transparent", fill="orange", alpha=0.3)+
  geom_bar(aes(x = Period, y = ET0_har, col = sign, fill = sign),
           show.legend = F, stat = "identity") +
  scale_color_manual(values = c("pos" = "darkblue", "neg" = "red")) +
  scale_fill_manual(values = c("pos"  = "darkblue", "neg" = "red")) +
  ylab("SPEI") + ggtitle("9-Month SPEI") +
  theme(plot.title = element_text(hjust = 0.5))
SPEI9

DF12 <- zoo::fortify.zoo(spei12$fitted) %>%
  dplyr::select(-Index) %>% 
  dplyr::mutate(Period = as.Date(paste(df_meteo_mo$year, df_meteo_mo$month, "1", sep="-"))) %>% 
  na.omit() %>% 
  dplyr::mutate(sign = ifelse(ET0_har >= 0, "pos", "neg"))


SPEI12<-ggplot(DF12[-3,]) +
  geom_rect(data=rects, inherit.aes=FALSE,
            aes(xmin=start, xmax=end, ymin=min(DF6$ET0_har),
                ymax=max(DF6$ET0_har), group=group), 
            color="transparent", fill="orange", alpha=0.3)+
  geom_bar(aes(x = Period, y = ET0_har, col = sign, fill = sign),
           show.legend = F, stat = "identity") +
  scale_color_manual(values = c("pos" = "darkblue", "neg" = "red")) +
  scale_fill_manual(values = c("pos"  = "darkblue", "neg" = "red")) +
  ylab("12-month SPEI") +
  xlab(NULL)+
  theme(plot.title = element_text(hjust = 0.5))+
  xlim(as.Date("1998-10-01"), as.Date("2019-12-01"))

SPEI12

Arkflow_monthly<- ggplot(data=monthly_Ark) +
  geom_line(aes(x=date, y= log10(Q_cms_sum)))+
  geom_rect(data=rects, inherit.aes=FALSE,
            aes(xmin=start, xmax=end, ymin=min(log10(monthly_Ark$Q_cms_sum)),
                ymax=max(log10(monthly_Ark$Q_cms_sum)), group=group), 
            color="transparent", fill="orange", alpha=0.3)+
  labs(x = "Date",
       y = "Log10 Monthly\nDischarge (cms)")

CombinedSPEI <- spei_inputs / SPEI3 / SPEI12 / Arkflow_monthly

CombinedSPEI + plot_annotation(tag_levels = 'A')

ggsave(file.path('plots', "CombinedSPEI.PNG"),
       width = 8, height = 6, units = "in")
