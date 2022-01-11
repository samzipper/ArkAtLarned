#Pumping_Data_Processing.R

source(file.path("code", "paths+packages.R"))
data_path<-file.path("data", "WIMAS_Larned.CSV")

WIMAS<-readr::read_csv(data_path, col_types = cols())
WIMAS2<-mutate_all(WIMAS, function(x) as.numeric(as.character(x)))
Years<-c(1998:2018)


##Create new dataframe for summary stats.  Loops through Year columns to sum and average the year
WIMAS_summary<-as.data.frame(Years)

j=1
for ( i in 22:42){
  temp<-lapply(WIMAS[,i], as.numeric)

  
  WIMAS_summary$AvgWater_AF[j]<-mean(unlist(temp))
  WIMAS_summary$SumWater_AF[j]<-sum(unlist(temp))
  j<-j+1
}

##Convert from Acre Feet to cubic meters
WIMAS_summary$AvgWater_m3<-WIMAS_summary$AvgWater_AF * 1233.48
WIMAS_summary$SumWater_m3<-WIMAS_summary$SumWater_AF * 1233.48

WIMAS_summary$date<-as.Date(with(WIMAS_summary, paste(Years, 1, 1,sep="-")), "%Y-%m-%d")

readr::write_csv(WIMAS_summary, file.path("data", "WIMAS_AnnualSummary_Larned.CSV"))
