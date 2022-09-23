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
# rm(list=ls())
dir.create("Results",recursive = T,showWarnings = F)
#Shape file
setwd("/Climate-Agriculture-1/Drought_Monitoring_Package")

Africa<-readOGR("/ACMAD_Git/SHP_AFRIQUE/Afrique_frontier_news.shp") 



# Month="Jan"
# Month_name="January"
# Year=2022

download.file(paste("http://iridl.ldeo.columbia.edu/expert/expert/SOURCES/.NOAA/.NCEP/.CPC/.GMSM/.w/Y/-40/0.5/40/GRID/X/-25/0.5/55/GRID/T/(%20",Month,"%20",Year,")/VALUES/data.nc",sep=""),mode="wb",paste("/ACMAD_Git/Drought_Monitoring_Package/Data/Soil_M_",Month,"","_",Year,".nc"))

SM_Raster_format<-raster::raster(x =paste("/ACMAD_Git/Drought_Monitoring_Package/Data/Soil_M_",Month,"","_",Year,".nc"))

# Non_Interpolated_SM<-as.data.frame(rasterToPoints(SM_Raster_format))
# Non_Interpolated_SM$w<-ifelse(Non_Interpolated_SM$w<0,NA,Non_Interpolated_SM$w)
# 
# names(Non_Interpolated_SM)[3]="SM"

Data_interpolted_SM<-raster::disaggregate(SM_Raster_format,5,method="bilinear")

Data_Masked_SM<-raster::mask(Data_interpolted_SM,Africa)

Data_df_SM<- as.data.frame(rasterToPoints(Data_Masked_SM))

#Data_df_SM$w<-ifelse(Data_df_SM$w<0,NA,Data_df_SM$w)

names(Data_df_SM)[3]="SM"

# Data<-nc_open(filename = paste("Data/Soil_M_",Month,"","_",Year,".nc"))
# Lon<-ncvar_get(Data,"X")
# Lat<-ncvar_get(Data,"Y")
# Val<-ncvar_get(Data,"w")
# nc_close(Data)
# 
# for(i in 1:length(Lat)){
#   if(i==1){
#     Soil_M<-data.frame(Lon=Lon,Lat=Lat[i],Soil=Val[,i])
#   }
#   else{
#     Soil_M1<-data.frame(Lon=Lon,Lat=Lat[i],Soil=Val[,i])
#     Soil_M<-rbind(Soil_M,Soil_M1)
#   }
# }

#Climatology Soil Moisture

download.file(paste("http://iridl.ldeo.columbia.edu/expert/expert/SOURCES/.NOAA/.NCEP/.CPC/.GMSM/.w/T/(",Month,"%201981)/(",Month,"%202010)/RANGEEDGES/Y/-40/0.5/40/GRID/X/-25/0.5/55/GRID/T/12/STEP/%5BT%5Daverage/data.nc",sep=""),mode="wb",paste("/ACMAD_Git/Drought_Monitoring_Package/Data/Climatology_Soil_",Month,"","_",Year,".nc"))



SM_Raster_format_Clim<-raster::raster(x =paste("/ACMAD_Git/Drought_Monitoring_Package/Data/Climatology_Soil_",Month,"","_",Year,".nc"))

# Non_Interpolated_Clim<-as.data.frame(rasterToPoints(SM_Raster_format_Clim))
# 
# names(Non_Interpolated_Clim)[3]="SM_Clim"
# 
# Non_Interpolated_Clim<-merge(Non_Interpolated_Clim,Non_Interpolated_SM,by=c("x","y"))
# 
# 
# Non_Interpolated_Clim$Anomaly<-Non_Interpolated_Clim$SM-Non_Interpolated_Clim$SM_Clim
# 
# rio::export(Non_Interpolated_Clim[,c("x","y","Anomaly")],paste("Results/Anomaly_Soil_",Month,".csv",sep=""))


SM_Data_interpolted_Clim<-raster::disaggregate(SM_Raster_format_Clim,5,method="bilinear")

SM_Data_Masked_Clim<-raster::mask(SM_Data_interpolted_Clim,Africa)

#plot(SM_Data_Masked_Clim)

SM_Data_df_Clim<- as.data.frame(rasterToPoints(SM_Data_Masked_Clim))


names(SM_Data_df_Clim)[3]="SM_Clim"

# Data<-nc_open(filename = paste("Data/Climatology_Soil_",Month,"","_",Year,".nc"))
# Lon<-ncvar_get(Data,"X")
# Lat<-ncvar_get(Data,"Y")
# Val<-ncvar_get(Data,"w")
# nc_close(Data)
# 
# for(i in 1:length(Lat)){
#   if(i==1){
#     Climatology_Soil<-data.frame(Lon=Lon,Lat=Lat[i],Climatology_S=Val[,i])
#   }
#   else{
#     Climatology_Soil1<-data.frame(Lon=Lon,Lat=Lat[i],Climatology_S=Val[,i])
#     Climatology_Soil<-rbind(Climatology_Soil,Climatology_Soil1)
#   }
# }
#######################Soil Moisture Anomaly####################################
Anomaly_Soil<-merge(Data_df_SM,SM_Data_df_Clim,by=c("x","y"))

Anomaly_Soil$Anomaly<-ifelse(Anomaly_Soil$SM<0,0,Anomaly_Soil$SM-Anomaly_Soil$SM_Clim)

rio::export(Anomaly_Soil,paste("/ACMAD_Git/Drought_Monitoring_Package/Results/Anomaly_Soil_",Month,".csv",sep=""))



#####################Mapping





mybreaks <- c(seq(-120,-30,30),-10,10,seq(30,180,30))
mybreaks<-c(-Inf,mybreaks,Inf)
#Function to return the dersired number of colors
mycolors<- function(x) {
  colors<-colorRampPalette(c("#5b3c11","#5b3c11","#5b3c11","#775935","#93775a","#c9b9a9","#f5fcfe","#f5fcfe","#b1cdbe","#8bb49f","#659c81","#3e8464","#006c48","#006c48"))(13)
  colors[1:x]
}

#Function to create labels for legend
breaklabel <- function(x){
  labels<- c("-150",c(seq(-120,-30,30),-10,0,10,seq(30,150,30)))
  labels[1:x]
}

Title =paste("SOIL MOISTURE ANOMALY OF ",toupper(Month_name)," ",Year,"\nData Source: NOAA NCEP CPC GMSM",sep="")

Im<-grid::rasterGrob(png::readPNG("Logos/Acmad_logo_1.png"), interpolate = TRUE)

l<-ggplot()+geom_contour_filled(data=Anomaly_Soil, aes(x,y,z = Anomaly),breaks= mybreaks, show.legend = TRUE) +
  scale_fill_manual(palette=mycolors, values=breaklabel(13), name="", drop=FALSE, guide = guide_legend(reverse = TRUE))+theme_bw()

last<-l+geom_polygon(data = Africa, aes(x = long,y = lat, group = group), fill = NA,color = "black",size = 1.1)+ theme(legend.position = c(.09, .09),legend.justification = c("left", "bottom"),legend.box.just = "right",legend.margin = margin(6, 6, 6, 6),legend.text = element_text(size=20,face = "bold"),plot.title = element_text(hjust = 0.5,size=15,face = "bold"),axis.text.x = element_text(size=15,face = "bold"),axis.text.y = element_text(size=15,face = "bold"))
last<-last+  annotation_custom(Im, xmin = 50, xmax = 60, ymin =30, ymax = 40) +coord_cartesian(clip = "off")
last<-last+ metR::scale_x_longitude(limits = c(-25, 60),breaks = seq(-25, 60,10)) + metR:: scale_y_latitude(limits = c(-40, 40),breaks = seq(-40, 40,10))
last<-last+labs(title = Title,x="",y="")

dir.create(paste("Products/Graphs/",Year,"/",Month_name,sep=""),recursive = T,showWarnings = F)
jpeg(filename = paste("Products/Graphs/",Year,"/",Month_name,"/Soil_Moisture_",Month,"_",Year,".jpeg",sep=""),
     width = 11,
     height = 12,
     units = "in",
     res=300)
print(last)
dev.off()


