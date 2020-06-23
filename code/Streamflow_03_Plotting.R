#Streamflow_03_Plotting.R

source(file.path("code", "paths+packages.R"))

#Read in Baseflow DataSet
data_path<- file.path("data", "ArkLarned_Baseflow.CSV")

if (file.exists(data_path)){
 
  Ark_df<-readr::read_csv(data_path, col_types = cols())
  year_df<-dplyr::group_by(Ark_df, year)
  month_df<-dplyr::group_by(Ark_df, month, year) %>%
    dplyr::summarise(monthly_q=mean(discharge_cms))
  
  DOY_df<-dplyr::group_by(Ark_df, DOY)
  doy<-data.frame(c(1:366))
  DOY<-
    DOY_df %>%
    dplyr::summarise(DOY_avg_q = mean(discharge_cms)) 
  baseflow<-dplyr::summarise(DOY_df, DOY_avg_bt = mean(bt)) 
  DOY<-cbind(DOY, baseflow$DOY_avg_bt)
  DOY<-magrittr::set_colnames(DOY, c('DOY', 'DOY_avg_q', "bf"))
   
 
  DOY$DOY <- unlist(DOY$doy)
  DOY$bt<-unlist(DOY$bt)
  
  #Discharge per DOY of all Years, and aerage in blue line
  ggplot() +
    geom_point(data=DOY_df, aes(x=DOY, y=discharge_cms, group=year))+
    geom_line(data=DOY, aes(x=DOY, y=DOY_avg_q), color=col.cat.blu)+
    geom_line(data=DOY, aes(x=DOY, y = bf), color=col.cat.red)

  
  #Discharge over time 
  month_df %>%
    ggplot(aes(x=year, y=monthly_q))+
    geom_point()+
    geom_line()+
  facet_wrap(~ month, ncol = 3, scales = "free_y") +
    labs(title = "Monthly Flow Ark @ Larned 1998-2020",
         subtitle = "Data plotted by Month",
         y = "Flow (cms)",
         x = "Year") + theme_bw(base_size = 10)
  
  
  
}
