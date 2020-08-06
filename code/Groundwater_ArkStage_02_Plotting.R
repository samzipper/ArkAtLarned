#####Groundwater_ArkStage_02_Plotting.R

source(file.path("code", "paths+packages.R"))
data_path<-file.path("data", "Ark_and_Aquifer_TimeSeries.CSV")
Ark_System<-readr::read_csv(data_path, col_types = cols())




####TimeSeries of River, Alluvial Aq, and HPA
Timeseries<-ggplot(data=Ark_System)+
  geom_point(aes(x=Date, y=Stage_Elev_m))+
  geom_point(aes(x=Date, y=Alluvial_Elev_m), color = col.cat.blu)+
  geom_point(aes(x=Date, y=HPA_Elevation_m), color = col.cat.red)+
  ylab("Elevation (m Above Sea Level")

Timeseries

  
mid<-mean.POSIXct(Ark_System$Date)

ggplot(data=Ark_System, aes(x=HPA_Elevation, y=Alluvial_Elev, color = Date))+
  geom_point()+
  scale_color_viridis()+
  ggtitle("Alluvial Aquifer Water Table vs River Level")

ggplot(data=Ark_System, aes(x=Stage_Elev, y=Alluvial_Elev, color = Date))+
  geom_point()+
  scale_color_viridis()+
  ggtitle("Alluvial Aquifer Water Table vs Ark Stage")


ggplot(data=Ark_System, aes(x=Stage_Elev, y=HPA_Elevation, color = Date))+
  geom_point()+
  scale_color_viridis()+
  ggtitle("HPA Potentiometric Surface vs Ark Stage")



  
  