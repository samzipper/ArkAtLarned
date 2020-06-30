##Streamflow_02_CleanData+CalculateMetrics.R

#prep workspace
source(file.path("code", "paths+packages.R"))

#Read in DataSet
data_path<- file.path("data", "Historical_Larned_Arkflow.CSV")

  #Check if file exists
  if (file.exists(data_path)){
    #read in data frame
    stream_df <-
      readr::read_csv(data_path, col_types = cols())
    
    #Group Data by Month
    stream_monthly<-
      stream_df %>%
      dplyr::group_by(year, month) %>%
      dplyr::summarize(Date_mid = mean(Date),
                       Q_cms_mean = mean(discharge_cms))
    
    #Calculate frequency of No Flow
    #Group by year
    stream_df<-
      stream_df %>%
      dplyr::mutate(DOY = lubridate::yday(lubridate::ymd(Date))) %>%
      dplyr::group_by(year)
   
    
     dry_df<-stream_df%>%
       filter(discharge_cms==0)
    
    
    y=count(dry_df, year)
    year<-c(as.numeric(first(stream_df$year):last(stream_df$year)))
    n<-c(1:length(year))*0
    days<-as.numeric(lubridate::leap_year(year))+365 
  
    
   DryDays<-data.frame(year, n, days)
    
   DryDays2<-merge(DryDays, y, by.x = 'year', by.y = 'year', all.x = TRUE) 
   DryDays2[is.na(DryDays2)]<-0
   DryDays2$n.x<-DryDays2$n.y/DryDays2$days * 100 
   yearly_df<- DryDays2 %>%
     dplyr::select(year, 'n.x') %>%
     magrittr::set_colnames(c('Year', "Percent_Dry"))
    
    
   
    
  
   
    
  
#Build Table with columns DOY, Q year 1, ..., Q year n
  #Might make some analyses easier (Might be easier to account for leap years)



#Number of Discrete No Flow Periods
  #While loop through days to check for 3+ day periods of 0 flow
  heat_df<-heatwaveR::exceedance(stream_df, x=Date, y=discharge_cms, 0.00001, below = TRUE, minDuration = 3)
  dryPeriods<-(heat_df$exceedance) %>%
    dplyr::select(exceedance_no, index_start, index_end, duration, date_start, date_end) %>%
    magrittr::set_colnames(c("DryPeriod_No","index_start", "index_end", 
                             "duration", "date_start", "date_end")) %>%
    dplyr::mutate(DOY_start = lubridate::yday(date_start),
                  DOY_end= lubridate::yday(date_end),
                  year_start = lubridate::year(date_start),
                  year_end = lubridate::year(date_end),
                  year_dif= year_end - year_start,) %>%
    dplyr::group_by(year_start)
 #Group for histogram 1 month, 1-6 month, 6-12 month, 1+ year
   dryPeriods$box_len<-cut(dryPeriods$duration, c(0,30,180,365,1000))
   ggplot(data = dryPeriods, aes(x=box_len))+geom_bar()
   
  
  #group for histogram of season that the dry periods start
   getSeason <- function(input.date){
     numeric.date <- 100*month(input.date)+day(input.date)
     ## input Seasons upper limits in the form MMDD in the "break =" option:
     cuts <- base::cut(numeric.date, breaks = c(0,319,0620,0921,1220,1231)) 
     # rename the resulting groups (could've been done within cut(...levels=) if "Winter" wasn't double
     levels(cuts) <- c("Winter","Spring","Summer","Fall","Winter")
     return(cuts)
   }
   
   dryPeriods$season_start<-getSeason(dryPeriods$date_start)
   
   dryPeriods$season_end<-getSeason(dryPeriods$date_end)
   
  
 
#Average Length of No Flow
  #Code TBD
  Average_NoFlow<-c(0,0,0,0,50.75, 173.5, 161.5, 365, 172.5, 15, 6, 0, 0, 196, 365, 178, 178, 154.5, 73, 32.33, 40.33, 0) 

  #Determine When it dries out
  

 
  #Determine First day of Year dry
  FirstDayofYear_NoFlow<-c(0,0,0,0, 108, 1, 1, 1, 1, 1, 240, 0, 0, 169, 1, 1, 1, 1, 1, 1, 65, 0)
  
  yearly_df<-
    yearly_df %>% 
    dplyr::mutate(AveargeNoFlowLength = Average_NoFlow,
                  FirstDayofNoFlow=FirstDayofYear_NoFlow)
  
  FirstDOY_NOFlowPeriod<-dryPeriods$DOY_start
  #Determine average first day over all years
  
  
  ##Baseflow
 
    baseflow<- EcoHydRology::BaseflowSeparation(stream_df$discharge_cms)
 stream_df<-cbind(stream_df, baseflow)
  
 stream_df$percentbase<-stream_df$bt/stream_df$discharge_cms *100
  }


readr::write_csv(stream_df, file.path("data", "ArkLarned_Baseflow.CSV"))
readr::write_csv(dryPeriods, file.path("data", "ArkLarned_DryPeriods.CSV"))
readr::write_csv(yearly_df, file.path("data", "YearlySummary_ArkLarned.CSV"))





