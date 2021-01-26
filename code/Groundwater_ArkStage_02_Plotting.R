#####Groundwater_ArkStage_02_Plotting.R

source(file.path("code", "paths+packages.R"))
library(viridis)
library(ggpubr)
data_path<-file.path("data", "Ark_and_Aquifer_TimeSeries.CSV")
Ark_Alluv_Path<-file.path("data", "Ark_and_Alluvial_TimeSeries.CSV")
Ark_HPA_Path<-file.path("data", "Ark_and_HPATimeSeries.CSV")
Alluv_HPA_Path<-file.path("data", "Alluvial_and_HPATimeSeries.CSV")


##Compare Ark stage and Alluvial level
Ark_Alluv<-readr::read_csv(Ark_Alluv_Path, col_types = cols())

newArkAlluv<-Ark_Alluv[Ark_Alluv$Stage_Elev_m > 593.24,]
newArkAlluv$Diff_Riv_Alluv<-newArkAlluv$Stage_Elev_m - newArkAlluv$Alluvial_Elev_m

DoubleMass_Ark_Alluv<-ggplot(data=newArkAlluv, aes(x=Stage_Elev_m, y=Alluvial_Elev_m))+
  geom_point(aes(colour = Date))+
  theme(legend.position="none")+
  scale_color_viridis_c()+
  scale_x_continuous(position = "top")+
  labs(x = "Ark Stage", 
       y = "Alluvial aq Head")

ArkAlluvTime<-ggplot(data=newArkAlluv)+
  geom_point(aes(x=Date, y = Alluvial_Elev_m, colour = "Alluvial"))+
  geom_point(aes(x=Date, y = Stage_Elev_m, colour = "Ark"))

Ark_alluv_diffPlot<-ggplot(data=newArkAlluv)+
  geom_point(aes(x=Date, y=Diff_Riv_Alluv))+
  geom_hline(yintercept = 0, color = 'gray65')+
  labs(x = "Date",
       y = "River Stage - Alluvial aq Head")

Ark_alluv_diffPlot
ArkAlluvTime
  

####Compare Ark Stage and HPA level
Ark_HPA<-readr::read_csv(Ark_HPA_Path, col_types = cols())



newArkHPA<-Ark_HPA[Ark_HPA$Stage_Elev_m > 593.24,]
newArkHPA$Diff_Riv_HPA<-newArkHPA$Stage_Elev_m - newArkHPA$HPA_Elevation_m

DoubleMass_Ark_HPA<-ggplot(data=newArkHPA, 
                           aes(x=Stage_Elev_m, y=HPA_Elevation_m, color = date_ymd))+
  geom_point()+
  scale_color_viridis_c(trans = 'date',  guide = guide_colorbar(title = 'Date'))+
  scale_x_continuous(position = "top")+
  labs(x = "", 
       y = "HPA Head")

ArkHPATime<-ggplot(data=newArkHPA)+
  geom_point(aes(x=Date, y = HPA_Elevation_m, colour = "HPA"))+
  geom_point(aes(x=Date, y = Stage_Elev_m, colour = "Ark"))

Ark_HPA_diffPlot<-ggplot(data=newArkHPA)+
  geom_point(aes(x=Date, y=Diff_Riv_HPA))+
  geom_hline(yintercept = 0, color = 'gray65')+
  labs(x = "Date",
       y = "River Stage - HPA Head")

DoubleMass_Ark_HPA
ArkHPATime
Ark_HPA_diffPlot

####Compare Alluvial Aq head and HPA level
Alluv_HPA<-readr::read_csv(Alluv_HPA_Path, col_types = cols())

Alluv_HPA$Diff_Alluv_HPA<-Alluv_HPA$Alluvial_Elev_m- Alluv_HPA$HPA_Elevation_m

DoubleMass_Alluv_HPA<-ggplot(data=Alluv_HPA, aes(y=Alluvial_Elev_m, x=HPA_Elevation_m))+
  geom_point(aes(colour = Date))+
  theme(legend.position="none")+
  scale_color_viridis_c()+
  scale_x_continuous(position = "top")+
  labs(x = "HPA Head", 
       y = "")

AlluvHPATime<-ggplot(data=Alluv_HPA)+
  geom_point(aes(x=Date, y = HPA_Elevation_m, colour = "HPA"))+
  geom_point(aes(x=Date, y = Alluvial_Elev_m, colour = "Alluvial aq"))

Alluv_HPA_diffPlot<-ggplot(data=Alluv_HPA)+
  geom_point(aes(x=Date, y=Diff_Alluv_HPA))+
  geom_hline(yintercept = 0, color = 'gray65')+
  labs(x = "Date",
       y = "Alluvial aq Head - HPA Head")

DoubleMass_Alluv_HPA
AlluvHPATime
Alluv_HPA_diffPlot



ggpubr::ggarrange(DoubleMass_Ark_Alluv, DoubleMass_Alluv_HPA, DoubleMass_Ark_HPA,
                  labels = c("A", "B", "C"),
                  ncol = 2, nrow = 2)

ggsave(file.path('plots', "DoubleMassCurves2.png"),
       width = 8, height = 4, units = "in")


####Old Double MAss Curves
mid<-mean.POSIXct(Ark_System$Date)

HP_Alluv<-ggplot(data=Ark_System, aes(x=HPA_Elevation_m, y=Alluvial_Elev_m, color = date_ymd))+
  geom_point()+
  scale_color_viridis_c(trans = 'date',  
                        guide = guide_colorbar(title = 'Date'))+
  labs(x = "Ark Stage", 
       y = "High Plains aq\n Head")
 

Ark_Alluv<-ggplot(data=Ark_System, aes(x=Stage_Elev_m, y=Alluvial_Elev_m))+
  geom_point(aes(colour = Date))+
  theme(legend.position="none")+
  scale_color_viridis_c()+
  scale_x_continuous(position = "top")+
  labs(x = "Ark Stage", 
       y = "Alluvial aq Head")


Ark_HP<-ggplot(data=Ark_System, aes(x=Stage_Elev_m, y=HPA_Elevation_m, color = Date))+
  geom_point()+
  theme(legend.position="none")+
  scale_color_viridis_c()+
  scale_x_continuous(position = "top")+
  labs(x = 'High Plains aq Head',
       y = 'Alluvial aq Head')

ggpubr::ggarrange(Ark_Alluv, Ark_HP, HP_Alluv, 
          labels = c("A", "B", "C"),
          ncol = 2, nrow = 2)



ggsave(file.path('plots', "WaterLevels.png"),
       width = 8, height = 4, units = "in")

  
  