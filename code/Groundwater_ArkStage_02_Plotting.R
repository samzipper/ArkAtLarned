#####Groundwater_ArkStage_02_Plotting.R

source(file.path("code", "paths+packages.R"))
library(viridis)
library(ggpubr)
data_path<-file.path("data", "Ark_and_Aquifer_TimeSeries.CSV")
Ark_System<-readr::read_csv(data_path, col_types = cols())


Ark_System %>% naniar::replace_with_na_at(.vars ='Stage_Elev_m', condition = ~.x < 593.4)

####TimeSeries of River, Alluvial Aq, and HPA
Timeseries<-ggplot(data=Ark_System)+
  geom_point(aes(x=Date, y=Stage_Elev_m))+
  geom_point(aes(x=Date, y=Alluvial_Elev_m), color = col.cat.blu)+
  geom_point(aes(x=Date, y=HPA_Elevation_m), color = col.cat.red)+
  ylab("Elevation (m Above Sea Level")

Timeseries

  
mid<-mean.POSIXct(Ark_System$Date)

HP_Alluv<-ggplot(data=Ark_System, aes(x=HPA_Elevation_m, y=Alluvial_Elev_m, color = date_ymd))+
  geom_point()+
  scale_color_viridis_c(trans = 'date',  
                        guide = guide_colorbar(title = 'Date'))+
  labs(x = "Ark Stage", 
       y = "High Plains aq Head")
 

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

  
  