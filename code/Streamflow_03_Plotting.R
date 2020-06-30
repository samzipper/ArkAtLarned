#Streamflow_03_Plotting.R

source(file.path("code", "paths+packages.R"))

#Read in Baseflow DataSet
data_path<- file.path("data", "ArkLarned_Baseflow.CSV")

if (file.exists(data_path)){
 
  Ark_df<-readr::read_csv(data_path, col_types = cols())
  year_df<-dplyr::group_by(Ark_df, year)
  month_df<-dplyr::group_by(Ark_df, month, year) %>%
    dplyr::summarise(monthly_q=mean(discharge_cms),
                     monthly_base = mean(bt))
  
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
    geom_line(data=DOY, aes(x=DOY, y = bf), color=col.cat.red)+
    ggsave(file.path('results', "Annual_Discharge.PNG"),
           width = 8, height = 8, units = "in")

  
  #Discharge over time 
  month_df %>%
    ggplot(aes(x=year, y=monthly_base))+
    geom_point()+
    geom_line()+
  facet_wrap(~ month, ncol = 3, scales = "free_y") +
    labs(title = "Monthly BaseFlow Ark @ Larned 1998-2020",
         subtitle = "Data plotted by Month",
         y = "Flow (cms)",
         x = "Year") + theme_bw(base_size = 10)+
    ggsave(file.path('results', "Monthly_Flow_Over_Time.png"),
           width = 8, height = 8, units = "in")
  
  month_df$month<-as.factor(month_df$month)
    ggplot(data=month_df)+
    geom_boxplot(aes(x=month, y= monthly_base))
  
  Ark_df %>%
    ggplot() +
    geom_line(aes(x=Date, y= discharge_cms))
  
  p<-ggplot(data = Ark_df) +
    geom_line(aes(x=Date, y= bt), color='black')+
    #geom_line(aes(x=Date, y= qft...9), color = col.cat.red, linetype = 'dotted')+
    ggtitle("Ark Baseflow (Black) and Quick Flow (Red)")+ 
    xlab("Date")+
    ylab('Discharge (cms)')
plotly_build(p)
  
ggplot(data = Ark_df)+
  geom_point(aes(x=discharge_cms, y=bt...8))

ggplot(data = Ark_df)+
  geom_point(aes(x=discharge_cms, y=qft...9))

ggplot(data = Ark_df)+
  geom_point(aes(x=qft...9, y=bt...8))
}

dryPeriods %>%
  group_by(box_len)



########### FIGURE 3 ################
data_path2<- file.path("data", "YearlySummary_ArkLarned.CSV")
yearly_df<-read.csv(data_path2) 

#Figure 3a
Fig3a<- ggplot(data=yearly_df)+
  geom_point(aes(x=Year, y=Percent_Dry))+
  geom_line(aes(x=Year, y=Percent_Dry))+
  ylab('% of Year Dry')

##Figure 3b
Fig3b<-ggplot(data=dryPeriods)+
  geom_bar(aes(x=box_len))+
  xlab('Dry Period Duration')+
  scale_x_discrete(labels=c("0-1 Month", "1-6 Months", "6-12 Months", ">1 Year"))+
  ylab('Number of Occurrences')

##Figure 3c
Fig3c<-ggplot(dryPeriods)+
  geom_boxplot(aes(x= box_len, y=DOY_start))+
  xlab('Dry Period Durarion')+
  scale_x_discrete(labels=c("0-1 Month", "1-6 Months", "6-12 Months", ">1 Year"))+
  ylab('Day of Year Dry Period Starts')

##Figure 3 - Plotting
Fig3Full<-(Fig3a)/(Fig3b | Fig3c) 
Fig3Full + plot_annotation( tag_levels = 'a',
  title = "Discrete Dry Periods of the Arkansas River 1998-2019")&
  theme(plot.title = element_text(hjust = 0.5))

ggsave(file.path('plots', "Discrete_DryPeriods.png"),
       width = 8, height = 8, units = "in")
  

############ FIGURE 4 #############
data_path3<-file.path('data', 'ArkLarned_DryPeriods.CSV')
df_dryperiods<-read.csv(data_path3)

##Season that Dry Period Starts Bar Graph
Fig4a<-ggplot(data=df_dryperiods, aes(x=season_start))+
  geom_bar()+
  ylab("Number of Occurrences")+
  xlab('Season Dry Period Started')

##Season that Dry Period Ends Bar Graph
Fig4b<-ggplot(data = df_dryperiods, aes(x=season_end))+
  geom_bar()+
  ylab("Number of Occurrences")+
  xlab('Season Dry Period Ended')

#Full Fig 4
Fig4 <- Fig4a + Fig4b
Fig4 + plot_annotation( tag_levels = 'a',
                        title = "Seasonality of Dry Periods of the Arkansas River 1998-2019")&
  theme(plot.title = element_text(hjust = 0.5))

ggsave(file.path('plots', "Seasons_of_DryPeriods.png"),
       width = 8, height = 4, units = "in")




#Plot percentage of Baseflow over Time
year_df$year<-as.factor(year_df$year)
ggplot(year_df)+
  geom_boxplot(aes(x=year, y=percentbase))

Ark_df$monthcode<-paste(Ark_df$year,"-",Ark_df$month)
box<-ggplot(Ark_df)+
  geom_boxplot(aes(x=monthcode, y=bt))

plotly_build(box)

ggplot()+
  geom_point(aes(x=Ark_df$percentbase, y=Ark_df$discharge_cms))


ggplot(data=Ark_df)+
  geom_point(aes(x=DOY, y=bt))

ggplot(Ark_df)+
  geom_point(aes(x=DOY, y=qft))

ggplot(Ark_df)+
  geom_point(aes(x=qft, y=bt))

             