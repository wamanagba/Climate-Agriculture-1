
library(ncdf4)
library(dplyr)
library(rio)
library(ggplot2)
library(metR)
library(rgdal)
rm(list = ls())

Data_Source="CPC-UNIFIED"
setwd("/Climate-Agriculture-1/DrySpell/")
Africa<-readOGR("/ACMAD_Git/SHP_AFRIQUE/Afrique_frontier_news.shp") 

MinLon=-25
MaxLon=55
MinLat=-40
MaxLat=40


k=2000
cpt=7
A=c("Jan", "Feb", "Mar", "Apr", "May", "Jun" ,"Jul", "Aug", "Sep" ,"Oct", "Nov", "Dec")
  


  for (cpt in 1:12) {

   cpt1=(cpt+1)%%12
   if(cpt1==0) cpt1=12
   cpt2=(cpt+2)%%12
   if(cpt2==0) cpt2=12
   
   Season_Name=paste(substr(A[cpt],1,1),substr(A[cpt1],1,1),substr(A[cpt2],1,1),sep = "")
   Season<-c(A[cpt],A[cpt1],A[cpt2])
   DDD=data.frame()
   for (k in 1981:2010) {

  
  Data=rio::import(paste("/ACMAD_Git/Data/CPC-UNIFIED/DataBase/Number_DrySpell10/",Season_Name,"/",k,".csv",sep = ""))
  DDD=rbind(DDD,Data)

  
  print(paste("Data/CPC-UNIFIED/DataBase/Number_DrySpell10/",Season_Name,"/",k,".csv",sep = ""))
  
  
   }
   
   NumberDay=DDD%>%
     group_by(X,Y)%>%
     summarise(MeanNumberSpell10=mean(spell10),Total=sum(spell10))

   dir.create(paste("/ACMAD_Git/Data/CPC-UNIFIED/DataBase/Climatology/Number_DrySpell10",sep = ""),recursive = T,showWarnings = F)

   rio::export(NumberDay,paste("/ACMAD_Git/Data/CPC-UNIFIED/DataBase/Climatology/Number_DrySpell10/",Season_Name,".csv",sep = ""))
  
}

