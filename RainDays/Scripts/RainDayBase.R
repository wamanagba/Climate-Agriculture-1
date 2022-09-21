



library(ncdf4)
library(dplyr)
library(rio)
library(ggplot2)
library(metR)
library(rgdal)
rm(list = ls())

Data_Source="CPC-UNIFIED"

Africa<-readOGR("SHP_AFRIQUE/Afrique_frontier_news.shp") 
setwd("~/Desktop/ACMAD_Git/")
MinLon=-25
MaxLon=55
MinLat=-40
MaxLat=40


k=2000
cpt=7
A=c("Jan", "Feb", "Mar", "Apr", "May", "Jun" ,"Jul", "Aug", "Sep" ,"Oct", "Nov", "Dec")
for (k in 1981:2021) {
  
  Data<-rio::import(paste("Data/CPC-UNIFIED/CSV_Format/",k,".csv",sep=""))
  Data$Month=format(Data$T,"%b")
  #Data$Year=format(Data$T,"%Y")
  Data$rain[is.na(Data$rain)]=0
  Data$count=ifelse(Data$rain<2.5,0,1)
  
jj=1
  for (cpt in 1:12) {
   print(jj)
   jj=jj+1
   cpt1=(cpt+1)%%12
   if(cpt1==0) cpt1=12
   cpt2=(cpt+2)%%12
   if(cpt2==0) cpt2=12
   
  Season_Name=paste(substr(A[cpt],1,1),substr(A[cpt1],1,1),substr(A[cpt2],1,1),sep = "")
  
  Season<-c(A[cpt],A[cpt1],A[cpt2])

  
  Seasonal<-filter(Data,Month %in% Season)
    

    
  NumberDay=Seasonal%>%
    group_by(X,Y)%>%
    summarise(NbRainDay=sum(count))
  
  Mask=rio::import(paste("Data/CPC-UNIFIED/Mask/mask_ ",Season_Name," .csv",sep = ""))
  
  Mask=dplyr::select(Mask,-"Mean")
  
  NumberDay=merge(NumberDay,Mask,by=c("X","Y"))
  
  NumberDay=filter(NumberDay,mask>=1)
  NumberDay=dplyr::select(NumberDay,-'mask')
  
  dir.create(paste("Data/CPC-UNIFIED/DataBase/Number_RainDays/",Season_Name,sep = ""),recursive = T,showWarnings = F)
  
  rio::export(NumberDay,paste("Data/CPC-UNIFIED/DataBase/Number_RainDays/",Season_Name,"/",k,".csv",sep = ""))
  
  print(k)
  
  
  }
}

