library(ncdf4)
library(dplyr)
library(rio)
library(tidync)
rm(list = ls())

setwd("E:/ACMAD_Git/")

k=2011

for (k in 2011:2021) {
  Data_NC<-nc_open(paste("Data/CPC-UNIFIED/",k,".nc",sep=""))
  Data<-tidync(paste("Data/CPC-UNIFIED/",k,".nc",sep=""))%>%hyper_tibble(na.rm = F)
  Date=seq(as.Date(paste(k,"-01-01",sep="")),as.Date(paste(k,"-12-31",sep="")),by="days")
  X<-length(ncvar_get(Data_NC,"X"))
  Y<-length(ncvar_get(Data_NC,"Y"))
  Date_All=sort(rep(Date,X*Y),decreasing = F)
  Data$T<-Date_All

 dir.create("/Climate-Agriculture/Preprocessing/Products/CPC-UNIFIED/",recursive = T,showWarnings = F)
 rio::export(Data,paste("/Climate-Agriculture/Preprocessing/Products/CPC-UNIFIED/",k,".csv",sep=""))  
}


