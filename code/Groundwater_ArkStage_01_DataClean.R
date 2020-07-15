##Groundwater_ArkStage_01_DataClean.R

source(file.path("code", "paths+packages.R"))
data_path1<-file.path("data", "ArkLarned_Aquifer_River_Levels.xlsx")
data_path2<-file.path("data", "Historical_Larned_ArkFlow.CSV")

Alluvial_df<-readxl::read_excel(path = data_path1,sheet= "LEC1", ) %>%
  dplyr::select('Time & Date', "Elevation")



HPA_df<-readxl::read_excel(path = data_path1,sheet= "LEC2", ) %>%
  dplyr::select('Time & Date', "Elevation")

ArkRiver_df<-readxl::read_excel(path = data_path1,sheet= "Ark River", skip = 34)
ArkRiver_df<-ArkRiver_df[-1,] %>%
  set_colnames(c("Agency", "sit_no", "Date", "datetime", "tz_cd", "Timezone",
                 "Stage_height", "57333_00060", "Stage_Elev", "57334_00060_cd", "quality_cd"))
ArkRiver_df$Stage_Elev<-as.numeric(ArkRiver_df$Stage_Elev)


ggplot(data = ArkRiver_df)+
  geom_point(aes(x=Date, y=Stage_Elev))



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


RatingCurve<-ggplot(data=CombinedStageDischarge, aes(x=mean_height, y = discharge_cms))+
  geom_point()+
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), size = 1)+
  xlab("Mean Daily Stage Elevation")+
  ylab("Discharge (cms)")+
  labs(title="Ark at Larned Rating Curve")

test<-lm(CombinedStageDischarge$discharge_cms ~ poly(CombinedStageDischarge$mean_height, 2, raw=TRUE))
summary(test)
