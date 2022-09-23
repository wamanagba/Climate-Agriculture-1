library(rio)
library(dplyr)
library(ncdf4)
library(RColorBrewer)
library(metR)
library(ggplot2)
library(rgdal)
library(ggplot2)
library(tidyverse)
library(raster)
library(sp)
library(ggdark)
library(showtext)
library(ggrepel)
options(download.file.extra = '--no-check-certificate')
rm(list=ls())
setwd("C:/Users/Yacou/Desktop/ACMAD_Git/Drought_Monitoring_Package/")

dir.create("Results",recursive = T,showWarnings = F)
#Monthly data
# Parameters<-import("Parameter/Parameters.csv")

# setwd(paste(Parameters[4],sep=""))
# Month=Parameters[1]
# Month_name=Parameters[2]
# Year=Parameters[3]

# setwd(paste(Parameters[4],sep=""))
Month="May"
Month_name="May"
Year=2000
for (Year in 2018:2021) {
 

Checking<-as.numeric(format(as.Date(paste(Year,"-",Month,"-",01,sep=""), tryFormats = c("%Y-%b-%d")),"%m"))

if(Checking==1){
  
  First_Month<-format(as.Date(paste(Year,"-",11,"-",01,sep=""), tryFormats = c("%Y-%m-%d")),"%b")
  
  First_Month_Name<-format(as.Date(paste(Year,"-",11,"-",01,sep=""), tryFormats = c("%Y-%m-%d")),"%B")
  
}
if(Checking==2){
  
  First_Month<-format(as.Date(paste(Year,"-",12,"-",01,sep=""), tryFormats = c("%Y-%m-%d")),"%b")
  
  First_Month_Name<-format(as.Date(paste(Year,"-",12,"-",01,sep=""), tryFormats = c("%Y-%m-%d")),"%B")
  
}
if(Checking>=3){

First_Month<-format(as.Date(paste(Year,"-",Checking-2,"-",01,sep=""), tryFormats = c("%Y-%m-%d")),"%b")

First_Month_Name<-format(as.Date(paste(Year,"-",Checking-2,"-",01,sep=""), tryFormats = c("%Y-%m-%d")),"%B")

}
#RR

Percentage<-rio::import(paste("Results/Percentage_",Month,".csv",sep=""))

## SPI 3

SPI<-rio::import(paste("Results/SPI_",First_Month,"-",Month,".csv",sep=""))

## SM-A
Anomaly_Soil<-rio::import(paste("Results/Anomaly_Soil_",Month,".csv",sep=""))

#Merge
Drought_Index<-merge(SPI[,c("x","y","SPI")],Anomaly_Soil[,c("x","y","Anomaly")],by=c("x","y"))


Drought_Index<-merge(Drought_Index,Percentage[,c("x","y","RR")],by=c("x","y"))

rio::export(Drought_Index,paste("Results/Drought_Index_",Month,".csv",sep=""))

##Importatn Indication

##RR:Percentage

##Anomaly: Soil Moisture anomaly

##SPI: SPI

##NDVI not included yet: 

Drought_Index$Index<-0

Drought_Index$Mask_RR<-0

Drought_Index$Mask_SM<-0

Drought_Index$Mask_SPI<-0

#################
#Severe Drought#
#################
#RR%

Drought_Index$Mask_RR<-ifelse(Drought_Index$RR< 40,1,Drought_Index$Mask_RR)

#Drought_Index$Mask_RR<-ifelse(Drought_Index$RR>=50 & Drought_Index$RR<75,1,Drought_Index$Mask_RR)

#SPI

#Drought_Index$Mask_SPI<-ifelse(Drought_Index$SPI>= -2 & Drought_Index$SPI<= -1,1,Drought_Index$Mask_SPI)

Drought_Index$Mask_SPI<-ifelse(Drought_Index$SPI< -1.5, 1,Drought_Index$Mask_SPI)

#SM
#Drought_Index$Mask_SM<-ifelse(Drought_Index$Anomaly< -60,1,Drought_Index$Mask_SM)
Drought_Index$Mask_SM<-ifelse(Drought_Index$Anomaly< -60 ,1,Drought_Index$Mask_SM)

##################
#Moderate Drought#
##################

Drought_Index$Mask_RR<-ifelse(Drought_Index$RR>=40 & Drought_Index$RR<=75,2,Drought_Index$Mask_RR)

#SPI
Drought_Index$Mask_SPI<-ifelse(Drought_Index$SPI>= -1.5 & Drought_Index$SPI< -1,2,Drought_Index$Mask_SPI)

#SM
Drought_Index$Mask_SM<-ifelse(Drought_Index$Anomaly>= -60 & Drought_Index$Anomaly<= -10,2,Drought_Index$Mask_SM)


#############
#Low Drought#
#############
#RR%

Drought_Index$Mask_RR<-ifelse(Drought_Index$RR>75 & Drought_Index$RR<95,3,Drought_Index$Mask_RR)

#SPI
Drought_Index$Mask_SPI<-ifelse(Drought_Index$SPI>-1  & Drought_Index$SPI<0,3,Drought_Index$Mask_SPI)


#SM
Drought_Index$Mask_SM<-ifelse(Drought_Index$Anomaly< -10  & Drought_Index$Anomaly>=-30 ,3,Drought_Index$Mask_SM)

Drought_Index$Mask_SM<-ifelse(Drought_Index$Anomaly > -10, 0,Drought_Index$Mask_SM)

#################
#Severe Drought##
#################

Drought_Index$Index<-ifelse(Drought_Index$Mask_RR==1 & Drought_Index$Mask_SPI==1 & Drought_Index$Mask_SM==1,4,Drought_Index$Index)
# 
Drought_Index$Index<-ifelse(Drought_Index$Mask_RR==1 & Drought_Index$Mask_SPI==2 & Drought_Index$Mask_SM==1,4,Drought_Index$Index)
# 
Drought_Index$Index<-ifelse(Drought_Index$Mask_RR==2 & Drought_Index$Mask_SPI==1 & Drought_Index$Mask_SM==1,4,Drought_Index$Index)
# 
Drought_Index$Index<-ifelse(Drought_Index$Mask_RR==1 & Drought_Index$Mask_SPI==1 & Drought_Index$Mask_SM==2,4,Drought_Index$Index)
# 
Drought_Index$Index<-ifelse(Drought_Index$Mask_RR==3 & Drought_Index$Mask_SPI==1 & Drought_Index$Mask_SM==1,4,Drought_Index$Index)
# 
Drought_Index$Index<-ifelse(Drought_Index$Mask_RR==1 & Drought_Index$Mask_SPI==3 & Drought_Index$Mask_SM==1,4,Drought_Index$Index)
#
Drought_Index$Index<-ifelse(Drought_Index$Mask_RR==1 & Drought_Index$Mask_SPI==1 & Drought_Index$Mask_SM==3,4,Drought_Index$Index)

# 
##################
#Moderate Drought#
##################

Drought_Index$Index<-ifelse(Drought_Index$Mask_RR==2 & Drought_Index$Mask_SPI==2 & Drought_Index$Mask_SM==2,3,Drought_Index$Index)
# 
Drought_Index$Index<-ifelse(Drought_Index$Mask_RR==2 & Drought_Index$Mask_SPI==2 & Drought_Index$Mask_SM==1,3,Drought_Index$Index)
# 
 Drought_Index$Index<-ifelse(Drought_Index$Mask_RR==1 & Drought_Index$Mask_SPI==2 & Drought_Index$Mask_SM==2,3,Drought_Index$Index)
# 
 Drought_Index$Index<-ifelse(Drought_Index$Mask_RR==2 & Drought_Index$Mask_SPI==1 & Drought_Index$Mask_SM==2,3,Drought_Index$Index)
# 
 Drought_Index$Index<-ifelse(Drought_Index$Mask_RR==2 & Drought_Index$Mask_SPI==2 & Drought_Index$Mask_SM==3,3,Drought_Index$Index)
# 
 Drought_Index$Index<-ifelse(Drought_Index$Mask_RR==3 & Drought_Index$Mask_SPI==2 & Drought_Index$Mask_SM==2,3,Drought_Index$Index)
# 
# 
 Drought_Index$Index<-ifelse(Drought_Index$Mask_RR==2 & Drought_Index$Mask_SPI==3 & Drought_Index$Mask_SM==2,3,Drought_Index$Index)
#
 Drought_Index$Index<-ifelse(Drought_Index$Mask_RR==0 & Drought_Index$Mask_SPI==1 & Drought_Index$Mask_SM==1,3,Drought_Index$Index)
#
#
 Drought_Index$Index<-ifelse(Drought_Index$Mask_RR==1& Drought_Index$Mask_SPI==0 & Drought_Index$Mask_SM==1,3,Drought_Index$Index)

##################
#Low Drought#
##################

Drought_Index$Index<-ifelse(Drought_Index$Mask_RR==3 & Drought_Index$Mask_SPI==3 & Drought_Index$Mask_SM==3,2,Drought_Index$Index)
# 
Drought_Index$Index<-ifelse(Drought_Index$Mask_RR==3 & Drought_Index$Mask_SPI==3 & Drought_Index$Mask_SM==1,2,Drought_Index$Index)
# 
Drought_Index$Index<-ifelse(Drought_Index$Mask_RR==3 & Drought_Index$Mask_SPI==1 & Drought_Index$Mask_SM==3,2,Drought_Index$Index)
# 
Drought_Index$Index<-ifelse(Drought_Index$Mask_RR==1 & Drought_Index$Mask_SPI==3 & Drought_Index$Mask_SM==3,2,Drought_Index$Index)
# 
Drought_Index$Index<-ifelse(Drought_Index$Mask_RR==3 & Drought_Index$Mask_SPI==3 & Drought_Index$Mask_SM==2,2,Drought_Index$Index)
# 
Drought_Index$Index<-ifelse(Drought_Index$Mask_RR==3 & Drought_Index$Mask_SPI==2 & Drought_Index$Mask_SM==3,2,Drought_Index$Index)
# 
Drought_Index$Index<-ifelse(Drought_Index$Mask_RR==2 & Drought_Index$Mask_SPI==3 & Drought_Index$Mask_SM==3,2,Drought_Index$Index)
# 

Drought_Index$Index<-ifelse(Drought_Index$Index==0,1,Drought_Index$Index)

rio::export(Drought_Index[,c("x","y","RR","Anomaly","SPI")],paste("Results/Drought_Index_",Month,".csv",sep=""))


######Mapping
#Shape file

Africa<-readOGR("SHP_AFRIQUE/Afrique_frontier_news.shp") 

Drought_Index_Raster<-raster::rasterFromXYZ(Drought_Index[,c("x","y","Index")])

#Data<-Drought_Index

Drought_Index_Raster_interpolted<-raster::aggregate(Drought_Index_Raster,4)

Drought_Index__Masked<-raster::mask(Drought_Index_Raster_interpolted,Africa)

Data<-as.data.frame(rasterToPoints(Drought_Index__Masked))


# data<-data.frame(Data$Lon,Data$Lat,Data$Index)
# 
# 
# 
# coords = cbind(Data$Lon,Data$Lat)
# 
# 
# sp = SpatialPoints(coords,proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs"))
# 
# # make spatial data frame
# 
# Drought_Index = SpatialPointsDataFrame(coords = sp,data)
# 
# Drought_Index_spdf <- Drought_Index 
# 
# 
# Drought_Index_spdf<-Drought_Index_spdf[!is.na(over(Drought_Index_spdf, as(Africa, "SpatialPolygons"))), ]
# 
# 
# Drought_Index_df<-as.data.frame(Drought_Index_spdf)




mybreaks <-  c(1,2,3,4,Inf)

#Function to return the dersired number of colors
mycolors<- function(x) {
  colors<-colorRampPalette(c("darkgray","yellow","orange","#936a4f"))(4)
  colors[1:x]
}

#Function to create labels for legend
breaklabel <- function(x){
  labels<- c("No Drought","Low Intensity Drought","Moderate Intensity Drought","Severe Drought")
  labels[1:x]
}

Title = paste("AFRICA DROUGHT MONITOR FOR ",toupper(Month_name)," ",Year,sep="")

Im<-grid::rasterGrob(png::readPNG("Logos/Acmad_logo_1.png"), interpolate = TRUE)

l<-ggplot(data=Data,aes(x,y))+geom_contour_filled(data=Data, aes(x,y,z = Index),breaks= mybreaks, show.legend = TRUE) +
  scale_fill_manual(palette=mycolors, values=breaklabel(4), name="", drop=FALSE, guide = guide_legend(reverse = TRUE))+theme_bw()

last<-l+geom_polygon(data = Africa, aes(x = long,y = lat, group = group), fill = NA,color = "black",size = 1.1)+ theme(legend.position = c(.08, .08),legend.justification = c("left", "bottom"),legend.box.just = "right",legend.margin = margin(6, 6, 6, 6),legend.text = element_text(size=17,face = "bold"),plot.title = element_text(hjust = 0.5,size=15,face = "bold"),axis.text.x = element_text(size=15,face = "bold"),axis.text.y = element_text(size=15,face = "bold"))
last<-last+  annotation_custom(Im, xmin = 50, xmax = 60, ymin =30, ymax = 40) +coord_cartesian(clip = "off")
last<-last+ metR::scale_x_longitude(limits = c(-25, 60),breaks = seq(-25, 60,10)) + metR:: scale_y_latitude(limits = c(-40, 40),breaks = seq(-40, 40,10))
last<-last+labs(title = Title,x="",y="")
#last<-last+stat_subset(aes(subset = RR< 50),geom = "point", size = 1.1,color="blue")+stat_subset(aes(subset =( SPI<-2)),geom = "point", size = 1.1,color="red")
dir.create(paste("Products/Graphs/",Month_name,sep=""),recursive = T,showWarnings = F)

jpeg(filename = paste("Products/Graphs/",Month_name,"/Drought_Index_y",Month,"_",Year,".jpeg",sep=""),
     width = 11,
     height = 12,
     units = "in",
     res=300)
print(last)
dev.off()

}
