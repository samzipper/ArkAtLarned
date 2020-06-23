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
    
 
#Average Length of No Flow
  #average from above
  #In order to plot over time, keep the year

#Determine When it dries out
  #Determine First day of Year dry
  #Determine average first day over all years

}