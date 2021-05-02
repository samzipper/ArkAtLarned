#Pumping_02_Plotting.R

source(file.path("code", "paths+packages.R"))
data_path<-file.path("data", "WIMAS_AnnualSummary_Larned.CSV")
WIMAS_summary<-readr::read_csv(data_path, col_types = cols())

WIMAS_summary<-WIMAS_summary[-1,]


#Load in Dry Periods
data_path3<- file.path("data", "ArkLarned_DryPeriods.CSV")
dryPeriods<- readr::read_csv(data_path3, col_types = cols())
rects <- data.frame(start=dryPeriods$date_start, end=dryPeriods$date_end, group=seq_along(start))


##Time Series of Average Annual Water Pumping of pumps over 10km radius
ggplot(data=WIMAS_summary)+
  geom_line(aes(x=date, y=AvgWater_m3))


##Time Series of Cummulative Water Pumping of pumps in 10km radius
  #Plots are the same but with a different y axis values

YearlyPump<-ggplot(data=WIMAS_summary)+
  geom_rect(data=rects, inherit.aes=FALSE,
            aes(xmin=start, xmax=end, ymin = min(WIMAS_summary$SumWater_m3),
                ymax= max(WIMAS_summary$SumWater_m3), group=group), 
            color="transparent", fill="orange", alpha=0.3)+
  geom_line(aes(x=date, y=SumWater_m3))+
  geom_point(aes(x=date, y=SumWater_m3))+
  xlim(as.Date("1999-01-01"),as.Date("2019-1-1"))+
  labs(y="Water Extracted (m3)",
       x=NULL)
  
YearlyPump

###Annual Meteorologic Trends
data_path4<- file.path("data", "AnnualWeather.CSV")
AnnualWeather<-readr::read_csv(data_path4, col_types = cols())
AnnualWeather<-AnnualWeather[-1,]

YearlyPrecip<-ggplot(AnnualWeather, aes(x = date, y = prcp_mm)) +
  geom_rect(data=rects, inherit.aes=FALSE,
            aes(xmin=start, xmax=end, ymin = min(AnnualWeather$prcp_mm),
                ymax= max(AnnualWeather$prcp_mm), group=group), 
            color="transparent", fill="orange", alpha=0.3)+
  geom_line() +
  geom_point()+
  labs(y="Precipitation (mm)",
       x =NULL)

YearlyPrecip
WIMAS_summary<-left_join(WIMAS_summary, AnnualWeather, by = 'date')


Annual_pump<-ggplot(data = WIMAS_summary, aes(x= prcp_mm, y = SumWater_m3))+
  geom_point()+
  geom_smooth(method = lm, se = FALSE)

eq<-lm(SumWater_m3 ~ prcp_mm, WIMAS_summary)

summary(eq)
#R2 is 0.3081

data_path5<- file.path("data", "YearlySummary_ArkLarned.CSV")
yearly_df<-read.csv(data_path5) 
yearly_df$date<-as.Date(with(yearly_df, paste(Year, 1, 1, sep="-")))
yearly_df<-yearly_df[-1,]

#Figure of PErcent Dry in the Year
Percent_Dry<- ggplot(data=yearly_df)+
  geom_rect(data=rects, inherit.aes=FALSE,
            aes(xmin=start, xmax=end, ymin = min(yearly_df$Percent_Dry),
                ymax= max(yearly_df$Percent_Dry), group=group), 
            color="transparent", fill="orange", alpha=0.3)+
  geom_point(data=yearly_df, aes(x=date, y=Percent_Dry))+
  geom_line(data=yearly_df, aes(x=date, y=Percent_Dry))+
  labs(x='Date',
       y= "% of Year Dry")
Percent_Dry


YearlyPrecip / YearlyPump / Percent_Dry + plot_annotation(tag_levels = 'A')


ggsave(file.path('plots', "YearlyRiverPumpRain2.png"),
       width = 8, height = 6, units = "in")

###Compare pumping, precip, and %dry

YearlySummary<-left_join(x=WIMAS_summary,y=yearly_df, by = "date")
#Pumping vs Precip

precip_v_pump.lm<-lm(YearlySummary$prcp_mm~YearlySummary$SumWater_m3)
summary(precip_v_pump.lm)$r.squared
precip_v_pump<- ggplot(data=YearlySummary, aes(x=SumWater_m3, y = prcp_mm)) +
  geom_point()+
  labs(y=" \n ",
       x = "Yearly Pumping (m3)")+
  geom_smooth(method = "lm", se=FALSE)+
  geom_text(y=900, x = 22000000, label="R^2 == 0.34", parse = TRUE, colour = 'red')

precip_v_pump

#Pumping vs %Dry

pump_v_dry.lm<-lm(YearlySummary$SumWater_m3~YearlySummary$Percent_Dry)
summary(pump_v_dry.lm)$r.squared
pump_v_dry<- ggplot(data=YearlySummary, aes(x=Percent_Dry, y = SumWater_m3)) +
  geom_point()+
  labs(y="Yearly Pumping (m3)",
       x = "% Year Dry")+
  geom_smooth(method = "lm", se=FALSE)+
  geom_text(x=15, y = 35000000, label="R^2 == 0.021", parse = TRUE, colour = 'red')
pump_v_dry


#Precip vs %Dry

precip_v_dry.lm<-lm(YearlySummary$prcp_mm~YearlySummary$Percent_Dry)
summary(precip_v_dry.lm)$r.squared
precip_v_dry<- ggplot(data=YearlySummary, aes(x=Percent_Dry, y = prcp_mm)) +
  geom_point()+
  labs(y="Yearly \nPrecipitation (mm)\n",
       x = NULL)+
  geom_smooth(method = "lm", se=FALSE)+
  geom_text(x=15, y = 900, label="R^2 == 0.0002", parse = TRUE, colour = 'red')

precip_v_dry

ggpubr::ggarrange(precip_v_dry, precip_v_pump, pump_v_dry, 
                  labels = c("A", "B", "C"),
                  ncol = 2, nrow = 2)

precip_v_dry + precip_v_pump + pump_v_dry + plot_spacer()+
  plot_layout(ncol = 2)



ggsave(file.path('plots', "PumpPrecipDryCorrelate.png"),
       width = 8, height = 4, units = "in")
