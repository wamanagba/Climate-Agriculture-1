
library(mondate)
# # ff=Data
# Instat=as.data.frame(Data$rain)
# # Instat=filter(Instat)
# # Data=Instat$`2022`
# Data=Instat
Function_speel=function(Data){
Data=as.data.frame(Data)
# last_row = tail(Data, n =729330)
# last_row = tail(Data, n =27)
# date_1 = as.Date(lubridate::floor_date(Sys.Date(), unit = "month"))
# date_2=as.Date((mondate(Sys.Date()-2)))
# #date_2 = Last_Date
# diff_dates =as.numeric(difftime(date_2,date_1, units = "days"))+1
# #Data=filter(Data,Days<=diff_dates)
# last_row$Days=seq(1,diff_dates,1)

colnames(Data)[1]="prcp"
Data[is.na(Data)] = 0
Data$cum=cumsum(Data$prcp)
ss=as.numeric(tail(Data, n =1)['cum'][1])
Data$val=round(Data$cum-ss,2)
Data=filter(Data,val>=-2.5)
#Date_end<-format(as.Date(Data$Days[1],origin="2016-12-31"),"%d-%b")
Length=length(Data$val)

return(Length)
}
# Function_speel(ff)
