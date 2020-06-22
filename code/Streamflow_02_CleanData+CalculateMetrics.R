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
      dplyr::group_by(Year, Month)
      dplyr::summarize(Date_mid = mean(Date),
                       Q_cms_mean = mean(discharge_cms))
  } 

#Build Table with columns DOY, Q year 1, ..., Q year n
  #Might make some analyses easier (Migh be easier to account for leap years)

#Calculate frequency of No Flow
  #Group by year
  for (i in num years){
    for (j in length of year){
      if (discharge == 0){
        drydays+=1
      }
      percent<-drydays/length [i]*100
      }
    }


#Number of Discrete No Flow Periods
  #While loop through days to check for 3+ day periods of 0 flow

#Average Length of No Flow
  #average from above
  #In order to plot over time, keep the year

#Determine When it dries out
  #Determine First day of Year dry
  #Determine average first day over all years

