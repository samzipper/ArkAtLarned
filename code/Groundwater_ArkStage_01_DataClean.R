##Groundwater_ArkStage_01_DataClean.R

source(file.path("code", "paths+packages.R"))
data_path1<-file.path("data", "ArkLarned_Aquifer_River_Levels.xlsx")
data_path2<-file.path("data", "Historical_Larned_ArkFlow.CSV")

data_path3<- file.path("data", "ArkLarned_DryPeriods.CSV")
dryPeriods<- readr::read_csv(data_path3, col_types = cols())
rects <- data.frame(start=dryPeriods$date_start, end=dryPeriods$date_end, group=seq_along(start))
rects$start<-as.POSIXct.Date(rects$start)
rects$end<-as.POSIXct.Date(rects$end)
rects$group<-(1:21)

Alluvial_df<-readxl::read_excel(path = data_path1,sheet= "LEC1", ) %>%
  dplyr::select('Time & Date', "Elevation")%>%
  set_colnames(c("Date", "Alluvial_Elev"))



HPA_df<-readxl::read_excel(path = data_path1,sheet= "LEC2", ) %>%
  dplyr::select('Time & Date', "Elevation") %>%
  set_colnames(c("Date", "HPA_Elevation"))

ArkRiver_df<-readxl::read_excel(path = data_path1,sheet= "Ark River", skip = 34)
ArkRiver_df<-ArkRiver_df[-1,] %>%
  set_colnames(c("Agency", "sit_no", "Date", "datetime", "tz_cd", "Timezone",
                 "Stage_height", "57333_00060", "Stage_Elev", "57334_00060_cd", "quality_cd"))
ArkRiver_df$Stage_Elev<-as.numeric(ArkRiver_df$Stage_Elev)





####Determining Rating Curve

ArkStage<-ArkRiver_df[120055:268160,]
ArkStage<-ArkStage %>%
  dplyr::mutate(year = lubridate::year(Date), 
                month = lubridate::month(Date), 
                day = lubridate::day(Date))
ArkStageDay <-
  ArkStage %>% 
  dplyr::group_by(year, month, day) %>% 
  dplyr::summarize(mean_height = mean(Stage_Elev))

ArkStageDay$Date<- paste(ArkStageDay$year, ArkStageDay$month, ArkStageDay$day, sep="-") %>% ymd() %>% as.Date()


Discharge_df<-readr::read_csv(data_path2, col_types = cols())
CutDischarge_df<-Discharge_df[3289:4840,]


CombinedStageDischarge<-left_join(CutDischarge_df, ArkStageDay)
RC_model<-lm(mean_height~I(sqrt(discharge_cms)), data=CombinedStageDischarge)



summary(RC_model)
predicted_df <- data.frame(height = predict(RC_model, CombinedStageDischarge),
                           discharge_cms=CombinedStageDischarge$discharge_cms)

RatingCurve<-ggplot(data=CombinedStageDischarge, aes(x = discharge_cms, y=mean_height))+
  geom_point()+
  geom_line(color='red',data = predicted_df, aes(x=discharge_cms, y=height))


  ylab("Mean Daily Stage Elevation")+
  xlab("Discharge (cms)")+
  labs(title="Ark at Larned Rating Curve")


RatingCurve  
###Rating Curve with height in meters

CombinedStageDischarge2<-left_join(CutDischarge_df, ArkStageDay)
CombinedStageDischarge2$mean_height_m<-CombinedStageDischarge2$mean_height*0.3048
RC_model_m<-lm(mean_height_m~I(sqrt(discharge_cms)), data=CombinedStageDischarge2)


summary(RC_model_m)
predicted_df2 <- data.frame(height = predict(RC_model_m, CombinedStageDischarge2),
                           discharge_cms=CombinedStageDischarge$discharge_cms)

RatingCurve<-ggplot(data=CombinedStageDischarge2, aes(x = discharge_cms, y=mean_height_m))+
  geom_point()+
  geom_line(color='red',data = predicted_df2, aes(x=discharge_cms, y=height))+
  ylab("Mean Daily Stage Elevation (m)")+
  xlab("Discharge (cms)")+
  labs(title="Ark at Larned Rating Curve")+
  annotate("text", x = 60, y =595,
           label = "paste(italic(R) ^ 2, \" = .92\")", parse = TRUE)


RatingCurve  
ggsave(file.path('plots', "RatingCurve.png"),
       width = 8, height = 4, units = "in")

######Apply Rating Curve for Missing ArK Data

Discharge_df$Stage_Elev<-RC_model$coefficients[1]+RC_model$coefficients[2]*sqrt(Discharge_df$discharge_cms)

Discharge_df$Date<-as.POSIXct(Discharge_df$Date)
Date<-seq.POSIXt(from = as.POSIXct("1998-10-01 00:00:00"), to = as.POSIXct("2019-12-31 23:45:00"), by = "15 min")


###MAke combined Dataframe for Both Ark Datasets and both Aquifers
Ark_System<-as.data.frame(Date)
Ark_System<-left_join(Ark_System, ArkRiver_df, by="Date")


ArkStage_Missing_Stage <- Ark_System$Date[is.na(Ark_System$Stage_Elev)]
for (d in ArkStage_Missing_Stage){
  if (sum(is.finite(Discharge_df$Stage_Elev[Discharge_df$Date==d])) > 0){
    Ark_System$Stage_Elev[Ark_System$Date==d] <- Discharge_df$Stage_Elev[Discharge_df$Date==d]
  }}
  

Ark_System<-full_join(Ark_System, HPA_df)
Ark_System<-full_join(Ark_System, Alluvial_df)

Ark_System$Stage_Elev_m<-Ark_System$Stage_Elev * 0.3048
Ark_System$HPA_Elevation_m<-Ark_System$HPA_Elevation *0.3048
Ark_System$Alluvial_Elev_m<-Ark_System$Alluvial_Elev * 0.3048

Ark_System$Year<-lubridate::year(Ark_System$Date)
Ark_System$date_ymd<-lubridate::date(Ark_System$Date)


######Added plotting because Dataset would not save correctly
Timeseries<-ggplot(data=Ark_System)+
  geom_point(aes(x=Date, y=Stage_Elev_m, colour = "Arkansas River Stage"))+
  geom_point(aes(x=Date, y=Alluvial_Elev_m, colour = "Alluvial Aquifer Head"))+
  geom_point(aes(x=Date, y=HPA_Elevation_m, colour = "HPA Head"))+
  ylab("Elevation (m)")+
  geom_rect(data=rects, inherit.aes=FALSE,
            aes(xmin=start, xmax=end, ymin=min(Ark_System$HPA_Elevation_m),
                ymax=max(Ark_System$Stage_Elev_m), group=group), 
            color="transparent", fill="orange", alpha=0.3)+
  labs(colour = "Unit",
       title = "Historical Water Levels")+
  geom_hline(yintercept = 593.2385, color = "gray36", size = 1)
  

Timeseries
ggsave(file.path('plots', "TimeSeries_B.PNG"),
       width = 10, height = 6, units = "in")

readr::write_csv(Ark_System, file.path("data", "Ark_and_Aquifer_TimeSeries.CSV"))


##Plotting the Timeseries without the Dry river values.  
##Streamebd elevattion determined by rating curve discharge = 0
Ark_System <- Ark_System %>% mutate(Stage_Elev_m = replace(Stage_Elev_m, 
                                                           Stage_Elev_m <= 593.24, NA))



Timeseriesc<-ggplot(data=Ark_System, aes(x=Date))+
  geom_rect(data=rects, inherit.aes=FALSE,
            aes(xmin=start, xmax=end, ymin=-Inf,
                ymax=+Inf, group=group), 
            color="transparent", fill="orange", alpha=0.3)+
  geom_point(aes(y=Stage_Elev_m, colour="a"))+
  geom_point(aes(y=Alluvial_Elev_m, colour ="b"))+
  geom_point(aes(y=HPA_Elevation_m, colour = "c"))+
  labs(title = "Historical Water Levels")+
  geom_hline(yintercept = 593.2385, color = "gray36", size = 1)+
  ylab("Elevation (m)")+
  scale_color_manual(name = "Unit",
                     values = c("a" = "#0082c8", "b" = "#f58231", "c" = "#e6194b"),
                     labels = c("Arkansas River", "Alluvial Aquifer", "High Plains Aquifer"))+
  theme(legend.position = "bottom")

Timeseriesc
ggsave(file.path('plots', "TimeSeries_C.PNG"),
       width = 10, height = 6, units = "in")
