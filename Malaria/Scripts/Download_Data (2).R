library(dplyr)
library(tidync)
library(ncdf4)
library(dplyr)
library(rio)
library(raster)
library(rgdal)
library(ggplot2)
library(metR)
rm(list = ls())
MinLon=-25
MaxLon=55
MinLat=-40
MaxLat=40

Africa<-readOGR("E:/ACMAD_Git/SHP_AFRIQUE/Afrique_frontier_news.shp") 
setwd("E:/Climate-Agriculture/Malaria/")
Data_source="NCEP-NCAR"
dir.create(paste("Data/",Data_source,"/CSV",sep = ""),recursive = T,showWarnings = F)

Month="Aug"

# Humidite Relative mensuelle
var="HR"
# Month="Jul"
Year="2022"
Name=paste(Month,"_",Year,"_",var,sep = "")
download.file(paste("http://iridl.ldeo.columbia.edu/expert/expert/SOURCES/.NOAA/.NCEP-NCAR/.CDAS-1/.MONTHLY/.Intrinsic/.PressureLevel/.rhum/P/(1000)/VALUES/Y/-40/2/40/GRID/X/-20/2/55/GRID/T/(",Month,"%20",Year,")/VALUES/data.nc",sep = ""),mode = "wb",paste("Data/",Data_source,"/",Name,".nc",sep = ""))
Data<-tidync(paste("Data/",Data_source,"/",Name,".nc",sep=""))%>%hyper_tibble(na.rm = F)
rio::export(Data,paste("Data/",Data_source,"/CSV/",Name,".csv",sep=""))
Data_HR=Data
Raster_file<-rasterFromXYZ(Data[c("X","Y","rhum")])
Raster_file_1=disaggregate(Raster_file,20,method='bilinear')
rr = raster::mask(Raster_file_1 ,Africa)
Data <- as.data.frame(rasterToPoints(rr ))
mybreaks <- c(0,20,40,60,80,100,Inf)
#Function to return the desired number of colors
mycolors<- function(x) {
  colors<-colorRampPalette(c("#F0FFF0","darkkhaki","#FFFF00","#FFA500","#FF0000","darkred"))(6)
  colors[1:x]
}
#Function to create labels for legend
breaklabel <- function(x){
  labels<- as.character(c(0,20,40,60,80,100))
  labels[1:x]
}
#############
Title<-toupper(paste("Monthly Relative Humidity from ",Month,"-",Year,"\nData Source: ",Data_source,sep=""))
l<-ggplot()+geom_contour_filled(data=Data, aes(x,y,z =rhum),breaks= mybreaks, show.legend = TRUE) +
  scale_fill_manual(palette=mycolors, values=breaklabel(6), name="Relative Humidity(%)", drop=FALSE, guide = guide_legend(reverse = T))+theme_bw()
last<-l+geom_polygon(data = Africa, aes(x = long,y = lat, group = group), fill = NA,color = "black",size = 1.1)+ theme(legend.position = c(.04, .04),legend.justification = c("left", "bottom"),legend.box.just = "right",legend.margin = margin(6, 6, 6, 6),legend.text = element_text(size=20),plot.title = element_text(hjust = 0.5,size=25,face = "bold"),axis.text.x = element_text(size=15,face = "bold"),axis.text.y = element_text(size=15,face = "bold"))
#last<-last+  annotation_custom(Im, xmin = MaxLon-5, xmax = MaxLon, ymin =MaxLat-5, ymax = MaxLat) +coord_cartesian(clip = "off")
last<-last+metR::scale_x_longitude(limits = c(MinLon,MaxLon),breaks =seq(MinLon,MaxLon,5))+scale_y_latitude(limits = c(MinLat,MaxLat),breaks =seq(MinLat,MaxLat,5))
last<-last+labs(title = Title,x="",y="")
dir.create(paste("Products/",Month,"/",sep=""),recursive = T,showWarnings = F)
jpeg(filename = paste("Products/",Month,"/",Year,"HR.jpeg",sep=""),
     width = 12,
     height =14,
     units = "in",
     res=300)
print(last)
dev.off()

#######################################################

########################################################
#Temperature mensuelle en surface 
var="TempM"
# Month="Jul"
Year="2022"
Name=paste(Month,"_",Year,"_",var,sep = "")

download.file(paste("http://iridl.ldeo.columbia.edu/expert/SOURCES/.NOAA/.NCEP-NCAR/.CDAS-1/.MONTHLY/.Diagnostic/.surface/.temp/(Celsius_scale)/unitconvert/Y/-40/2/40/GRID/X/-20/2/55/GRID/T/(",Month,"%20",Year,")/VALUES/data.nc"),mode = "wb",paste("Data/",Data_source,"/",Name,".nc",sep = ""))
Data<-tidync(paste("Data/",Data_source,"/",Name,".nc",sep=""))%>%hyper_tibble(na.rm = F)
rio::export(Data,paste("Data/",Data_source,"/CSV/",Name,".csv",sep=""))
Data_TP=Data

Raster_file<-rasterFromXYZ(Data[c("X","Y","temp")])
Raster_file_1=disaggregate(Raster_file,8,method='bilinear')
rr = raster::mask(Raster_file_1 ,Africa)
Data <- as.data.frame(rasterToPoints(rr ))
mybreaks <- c(0,5,10,15,20,25,30,35,40,Inf)
#Function to return the desired number of colors
mycolors<- function(x) {
  colors<-colorRampPalette(c("#2166AC","#4393C3","#92C5DE","#D1E5F0","#F7F7F7","#FDDBC7","#F4A582","#D6604D","#B2182B"))(9)
  colors[1:x]
}
#Function to create labels for legend
breaklabel <- function(x){
  labels<- as.character(c(0,5,10,15,20,25,30,35,40))
  labels[1:x]
}
#############
Title<-toupper(paste("Monthly surface temperature from ",Month,"-",Year,"\nData Source: ",Data_source,sep=""))
l<-ggplot()+geom_contour_filled(data=Data, aes(x,y,z =temp),breaks= mybreaks, show.legend = TRUE) +
  scale_fill_manual(palette=mycolors, values=breaklabel(9), name="Temperature(°C)", drop=FALSE, guide = guide_legend(reverse = T))+theme_bw()
last<-l+geom_polygon(data = Africa, aes(x = long,y = lat, group = group), fill = NA,color = "black",size = 1.1)+ theme(legend.position = c(.04, .04),legend.justification = c("left", "bottom"),legend.box.just = "right",legend.margin = margin(6, 6, 6, 6),legend.text = element_text(size=20),plot.title = element_text(hjust = 0.5,size=25,face = "bold"),axis.text.x = element_text(size=15,face = "bold"),axis.text.y = element_text(size=15,face = "bold"))
#last<-last+  annotation_custom(Im, xmin = MaxLon-5, xmax = MaxLon, ymin =MaxLat-5, ymax = MaxLat) +coord_cartesian(clip = "off")
last<-last+metR::scale_x_longitude(limits = c(MinLon,MaxLon),breaks =seq(MinLon,MaxLon,5))+scale_y_latitude(limits = c(MinLat,MaxLat),breaks =seq(MinLat,MaxLat,5))
last<-last+labs(title = Title,x="",y="")
#dir.create(paste("Products/malaria/",sep=""),recursive = T,showWarnings = F)
jpeg(filename = paste("Products/",Month,"/",Year,"Temp.jpeg",sep=""),
     width = 12,
     height =14,
     units = "in",
     res=300)
print(last)
dev.off()

#############################################################

###################################################
# MONTHLY PRECIPITATION

var="Prep"
# Month="Jul"
statDay=1
endday=31
Year="2022"
Name=paste(Month,"_",Year,"_",var,sep = "")
download.file(paste("http://iridl.ldeo.columbia.edu/expert/expert/SOURCES/.NOAA/.NCEP/.CPC/.FEWS/.Africa/.DAILY/.RFEv2/.est_prcp/T/(",statDay,"%20",Month,"%20",Year,")/(",endday,"%20",Month,"%20",Year,")/RANGEEDGES/Y/-40/0.5/40/GRID/X/-20/0.5/55/GRID/T/SUM/data.nc"),mode = "wb",paste("Data/",Data_source,"/",Name,".nc",sep = ""))

Data<-tidync(paste("Data/",Data_source,"/",Name,".nc",sep=""))%>%hyper_tibble(na.rm = F)
rio::export(Data,paste("Data/",Data_source,"/CSV/",Name,".csv",sep=""))


Raster_file<-rasterFromXYZ(Data[c("X","Y","est_prcp")])
Raster_file_1=disaggregate(Raster_file,8,method='bilinear')
rr = raster::mask(Raster_file_1 ,Africa)
Data <- as.data.frame(rasterToPoints(rr ))
mybreaks <- c(0,5,20,50,75,100,150,200,250,300,Inf)
mybreaks <- c(0,5,20,50,75,100,150,200,250,300,Inf)

#Function to return the desired number of colors
# mycolors<- function(x) {
#   colors<-colorRampPalette(c("#67001F", "#B2182B" ,"#D6604D" ,"#F4A582", "#FDDBC7", "#D1E5F0", "#92C5DE", "#4393C3" ,"#2166AC" ,"#053061"))(10)
#   colors[1:x]
# }

mycolors<- function(x) {
  colors<-colorRampPalette(c("gray66","#F7FBFF" ,"#DEEBF7" ,"#C6DBEF" ,"#9ECAE1","#6BAED6" ,"#4292C6" ,"#2171B5" ,"#08519C", "#08306B"))(10)
  colors[1:x]
}
#Function to create labels for legend
breaklabel <- function(x){
  labels<- as.character(c(0,5,20,50,75,100,150,200,250,300))
  labels[1:x]
}
#############
Title<-toupper(paste("MONTHLY PRECIPITATION  from ",Month,"-",Year,"\nData Source: ",Data_source,sep=""))
l<-ggplot()+geom_contour_filled(data=Data, aes(x,y,z =est_prcp),breaks= mybreaks, show.legend = TRUE) +
  scale_fill_manual(palette=mycolors, values=breaklabel(10), name="Precipitation (mm)", drop=FALSE, guide = guide_legend(reverse = T))+theme_bw()
last<-l+geom_polygon(data = Africa, aes(x = long,y = lat, group = group), fill = NA,color = "black",size = 1.1)+ theme(legend.position = c(.04, .04),legend.justification = c("left", "bottom"),legend.box.just = "right",legend.margin = margin(6, 6, 6, 6),legend.text = element_text(size=20),plot.title = element_text(hjust = 0.5,size=25,face = "bold"),axis.text.x = element_text(size=15,face = "bold"),axis.text.y = element_text(size=15,face = "bold"))
#last<-last+  annotation_custom(Im, xmin = MaxLon-5, xmax = MaxLon, ymin =MaxLat-5, ymax = MaxLat) +coord_cartesian(clip = "off")
last<-last+metR::scale_x_longitude(limits = c(MinLon,MaxLon),breaks =seq(MinLon,MaxLon,5))+scale_y_latitude(limits = c(MinLat,MaxLat),breaks =seq(MinLat,MaxLat,5))
last<-last+labs(title = Title,x="",y="")
jpeg(filename = paste("Products/",Month,"/",Year,"Precipitation.jpeg",sep=""),
     width = 12,
     height =14,
     units = "in",
     res=300)
print(last)
dev.off()


#################### Malaria maps##################

### Mask for the temperature #####

DATA=merge(Data_HR,Data_TP,by=c("X","Y"))
DATA=subset(DATA,select=-c(P,T.x,T.y))

DATA$Vigilance=4
DATA$mask_rh<-NA
DATA$mask_tmp<-NA

# masque RH

DATA$mask_rh<-ifelse(DATA$rhum>=80,1,DATA$mask_rh)
DATA$mask_rh<-ifelse(DATA$rhum>=70 & DATA$rhum<80,2,DATA$mask_rh)
DATA$mask_rh<-ifelse(DATA$rhum>=60 & DATA$rhum<=70,3,DATA$mask_rh)
DATA$mask_rh<-ifelse(DATA$rhum<60,4,DATA$mask_rh)

#masque temp
DATA$mask_tmp<-ifelse(DATA$temp>=25 & DATA$temp<=32,1,DATA$mask_tmp)

DATA$mask_tmp<-ifelse(DATA$temp>=20 & DATA$temp<25,2,DATA$mask_tmp)

DATA$mask_tmp<-ifelse(DATA$temp>=18 & DATA$temp<20,3,DATA$mask_tmp)

DATA$mask_tmp<-ifelse(DATA$temp<18,4,DATA$mask_tmp)

DATA$mask_tmp<-ifelse(DATA$temp>32,4,DATA$mask_tmp)


#Definition of mask of each vigilance level

# Vigilance elevee
DATA$Vigilance<-ifelse(DATA$mask_rh==1 & DATA$mask_tmp==1,1,DATA$Vigilance)
DATA$Vigilance<-ifelse(DATA$mask_rh==1 & DATA$mask_tmp==2,1,DATA$Vigilance)
DATA$Vigilance<-ifelse(DATA$mask_rh==2 & DATA$mask_tmp==1,1,DATA$Vigilance)

# Vigilance Moderee
DATA$Vigilance<-ifelse(DATA$mask_rh==2 & DATA$mask_tmp==2,2,DATA$Vigilance)
DATA$Vigilance<-ifelse(DATA$mask_rh==2 & DATA$mask_tmp==3,2,DATA$Vigilance)
DATA$Vigilance<-ifelse(DATA$mask_rh==3 & DATA$mask_tmp==2,2,DATA$Vigilance)
DATA$Vigilance<-ifelse(DATA$mask_rh==1 & DATA$mask_tmp==3,2,DATA$Vigilance)
DATA$Vigilance<-ifelse(DATA$mask_rh==3 & DATA$mask_tmp==1,2,DATA$Vigilance)

# Vigilance Faible
DATA$Vigilance<-ifelse(DATA$mask_rh==3 & DATA$mask_tmp==3,3,DATA$Vigilance)
DATA$Vigilance<-ifelse(DATA$mask_rh==3 & DATA$mask_tmp==4,3,DATA$Vigilance)
DATA$Vigilance<-ifelse(DATA$mask_rh==4 & DATA$mask_tmp==3,3,DATA$Vigilance)
DATA$Vigilance<-ifelse(DATA$mask_rh==1 & DATA$mask_tmp==4,3,DATA$Vigilance)
DATA$Vigilance<-ifelse(DATA$mask_rh==4 & DATA$mask_tmp==1,3,DATA$Vigilance)
DATA$Vigilance<-ifelse(DATA$mask_rh==2 & DATA$mask_tmp==4,3,DATA$Vigilance)
DATA$Vigilance<-ifelse(DATA$mask_rh==4 & DATA$mask_tmp==2,3,DATA$Vigilance)



# DATA$cond1=ifelse((DATA$temp>=25 & DATA$temp<=32)&(DATA$rhum>80),4,0)
# DATA$cond2=ifelse((DATA$temp>=20 & DATA$temp<25)&(DATA$rhum>=70 & DATA$rhum<=80),3,0)
# DATA$cond3=ifelse((DATA$temp>=18 & DATA$temp<20)&(DATA$rhum>=60 & DATA$rhum<70),2,0)
# DATA$cond4=ifelse((DATA$temp<18)&(DATA$rhum<60),1,0)
# DATA$cond5=DATA$cond1+DATA$cond2+DATA$cond3+DATA$cond4

var="Malaria"
# Month="Jun"
Year="2022" 
Name=paste(Month,"_",Year,"_",var,sep = "")
Raster_file<-rasterFromXYZ(DATA[c("X","Y","Vigilance")])
Raster_file_1=disaggregate(Raster_file,8,method='bilinear')
rr = raster::mask(Raster_file_1 ,Africa)
DATA <- as.data.frame(rasterToPoints(rr ))
DATA$Vigilance=round(DATA$Vigilance,0)
mybreaks <- c(1,2,3,4,Inf)
#Function to return the desired number of colors
mycolors<- function(x) {
  colors<-colorRampPalette(c("#FF0000","#FFA500","#FFFF00","gray66"))(4)
  colors[1:x]
}
#Function to create labels for legend
breaklabel <- function(x){
  labels<- as.character(c("High Vigilance","Moderate vigilance","Low vigilance",'No Vigilance'))
  labels[1:x]
}
#############
Title<-toupper(paste("Malaria Vigilance from ",Month,"-",Year,"\nData Source: ",Data_source,sep=""))
l<-ggplot()+geom_contour_filled(data=DATA, aes(x,y,z =Vigilance),breaks= mybreaks, show.legend = TRUE) +
  scale_fill_manual(palette=mycolors, values=breaklabel(4), name="", drop=FALSE, guide = guide_legend(reverse = T))+theme_bw()
last<-l+geom_polygon(data = Africa, aes(x = long,y = lat, group = group), fill = NA,color = "black",size = 1.1)+ theme(legend.position = c(.04, .04),legend.justification = c("left", "bottom"),legend.box.just = "right",legend.margin = margin(6, 6, 6, 6),legend.text = element_text(size=20),plot.title = element_text(hjust = 0.5,size=25,face = "bold"),axis.text.x = element_text(size=15,face = "bold"),axis.text.y = element_text(size=15,face = "bold"))
#last<-last+  annotation_custom(Im, xmin = MaxLon-5, xmax = MaxLon, ymin =MaxLat-5, ymax = MaxLat) +coord_cartesian(clip = "off")
last<-last+metR::scale_x_longitude(limits = c(MinLon,MaxLon),breaks =seq(MinLon,MaxLon,5))+scale_y_latitude(limits = c(MinLat,MaxLat),breaks =seq(MinLat,MaxLat,5))
last<-last+labs(title = Title,x="",y="")
#dir.create(paste("Products/malaria/",sep=""),recursive = T,showWarnings = F)
jpeg(filename = paste("Products/",Month,"/",Year,"Vigilance.jpeg",sep=""),
     width = 12,
     height =14,
     units = "in",
     res=300)
print(last)
dev.off()












1

# # MONTHLY PRECIPITATION ANOMALY
# 
# var="PrepAnomaly"
# Month="May"
# statDay=1
# endday=31
# Year="2022"
# Name=paste(Month,"_",Year,"_",var,sep = "")
# link_HR=paste("http://iridl.ldeo.columbia.edu/expert/expert/SOURCES/.NOAA/.NCEP/.CPC/.CAMS_OPI/.v0208/.anomaly/.prcp/T/(days%20since%201960-01-01)/streamgridunitconvert/T/differential_mul/T/(",Month,"%20",Year,")/(",Month,"%20",Year,")/RANGEEDGES/Y/(40S)/(40N)/RANGEEDGES/X/(20W)/(55E)/RANGEEDGES/data.nc")
# download.file(link_HR,mode = "wb",paste("Data/",Data_source,"/",Name,".nc",sep = ""))
# Data<-tidync(paste("Data/",Data_source,"/",Name,".nc",sep=""))%>%hyper_tibble(na.rm = F)
# rio::export(Data,paste("Data/",Data_source,"/CSV/",Name,".csv",sep=""))
# 
# 
# 
# Raster_file<-rasterFromXYZ(Data[c("X","Y","aprod")])
# Raster_file_1=disaggregate(Raster_file,8,method='bilinear')
# rr = raster::mask(Raster_file_1 ,Africa)
# Data <- as.data.frame(rasterToPoints(rr ))
# mybreaks <- c(-200,-100,0,200,400,800,Inf)
# #Function to return the desired number of colors
# mycolors<- function(x) {
#   colors<-colorRampPalette(c("#8cb02c","#37fdf8","blue","#89522a","black","red"))(6)
#   colors[1:x]
# }
# #Function to create labels for legend
# breaklabel <- function(x){
#   labels<- as.character(c(-200,-100,0,200,400,800))
#   labels[1:x]
# }
# #############
# Title<-toupper(paste("MONTHLY PRECIPITATION ANOMALY FROM ",Month,"\nData Source: ",Data_source,sep=""))
# l<-ggplot()+geom_contour_filled(data=Data, aes(x,y,z =aprod),breaks= mybreaks, show.legend = TRUE) +
#   scale_fill_manual(palette=mycolors, values=breaklabel(6), name="", drop=FALSE, guide = guide_legend(reverse = T))+theme_bw()
# last<-l+geom_polygon(data = Africa, aes(x = long,y = lat, group = group), fill = NA,color = "black",size = 1.1)+ theme(legend.position = c(.04, .04),legend.justification = c("left", "bottom"),legend.box.just = "right",legend.margin = margin(6, 6, 6, 6),legend.text = element_text(size=20),plot.title = element_text(hjust = 0.5,size=25,face = "bold"),axis.text.x = element_text(size=15,face = "bold"),axis.text.y = element_text(size=15,face = "bold"))
# #last<-last+  annotation_custom(Im, xmin = MaxLon-5, xmax = MaxLon, ymin =MaxLat-5, ymax = MaxLat) +coord_cartesian(clip = "off")
# last<-last+metR::scale_x_longitude(limits = c(MinLon,MaxLon),breaks =seq(MinLon,MaxLon,5))+scale_y_latitude(limits = c(MinLat,MaxLat),breaks =seq(MinLat,MaxLat,5))
# last<-last+labs(title = Title,x="",y="")
# jpeg(filename = paste("Products/",Name,".jpeg",sep=""),
#      width = 14,
#      height =14,
#      units = "in",
#      res=300)
# print(last)
# dev.off()
# 
# 
# #Temperature de surface hebdomadaire
# 
# var="surface_temp_heb"
# Month="May"
# statDay=1
# endday=31
# Year="2022"
# Name=paste(Month,"_",Year,"_",var,sep = "")
# link_HR=paste("http://iridl.ldeo.columbia.edu/expert/expert/SOURCES/.NOAA/.NCEP-NCAR/.CDAS-1/.DAILY/.Diagnostic/.surface/.temp/(Celsius_scale)/unitconvert/T/(",statDay,"%20",Month,"%20",Year,")/(",endday,"%20",Month,"%20",Year,")/RANGEEDGES/Y/-40/0.5/40/GRID/X/-20/0.5/55/GRID/%5BT%5Daverage/data.nc")
# download.file(link_HR,mode = "wb",paste("Data/",Data_source,"/",Name,".nc",sep = ""))
# 
# Data<-tidync(paste("Data/",Data_source,"/",Name,".nc",sep=""))%>%hyper_tibble(na.rm = F)
# rio::export(Data,paste("Data/",Data_source,"/CSV/",Name,".csv",sep=""))
# 
# Raster_file<-rasterFromXYZ(Data[c("X","Y","temp")])
# Raster_file_1=disaggregate(Raster_file,8,method='bilinear')
# rr = raster::mask(Raster_file_1 ,Africa)
# Data <- as.data.frame(rasterToPoints(rr ))
# mybreaks <- c(5,10,15,20,25,30,Inf)
# #Function to return the desired number of colors
# mycolors<- function(x) {
#   colors<-colorRampPalette(c("#8cb02c","#37fdf8","blue","#89522a","black","red"))(6)
#   colors[1:x]
# }
# #Function to create labels for legend
# breaklabel <- function(x){
#   labels<- as.character(c(5,10,15,20,25,30))
#   labels[1:x]
# }
# #############
# Title<-toupper(paste("Temperature de surface hebdomadaire FROM ",Month,"\nData Source: ",Data_source,sep=""))
# l<-ggplot()+geom_contour_filled(data=Data, aes(x,y,z =temp),breaks= mybreaks, show.legend = TRUE) +
#   scale_fill_manual(palette=mycolors, values=breaklabel(6), name="", drop=FALSE, guide = guide_legend(reverse = T))+theme_bw()
# last<-l+geom_polygon(data = Africa, aes(x = long,y = lat, group = group), fill = NA,color = "black",size = 1.1)+ theme(legend.position = c(.04, .04),legend.justification = c("left", "bottom"),legend.box.just = "right",legend.margin = margin(6, 6, 6, 6),legend.text = element_text(size=20),plot.title = element_text(hjust = 0.5,size=25,face = "bold"),axis.text.x = element_text(size=15,face = "bold"),axis.text.y = element_text(size=15,face = "bold"))
# #last<-last+  annotation_custom(Im, xmin = MaxLon-5, xmax = MaxLon, ymin =MaxLat-5, ymax = MaxLat) +coord_cartesian(clip = "off")
# last<-last+metR::scale_x_longitude(limits = c(MinLon,MaxLon),breaks =seq(MinLon,MaxLon,5))+scale_y_latitude(limits = c(MinLat,MaxLat),breaks =seq(MinLat,MaxLat,5))
# last<-last+labs(title = Title,x="",y="")
# jpeg(filename = paste("Products/",Name,".jpeg",sep=""),
#      width = 14,
#      height =14,
#      units = "in",
#      res=300)
# print(last)
# dev.off()
# 
# #Vent Méridien hebdo
# 
# 
# var="meridional_wind"
# Month="May"
# statDay=29
# statmonth="May"
# endday=5
# endMonth="Jun"
# Year="2022"
# Name=paste(Month,"_",Year,"_",var,sep = "")
# link_HR=paste("http://iridl.ldeo.columbia.edu/expert/expert/SOURCES/.NOAA/.NCEP-NCAR/.CDAS-1/.DAILY/.Intrinsic/.PressureLevel/.v/X/(40W)/(72E)/RANGEEDGES/T/(",statDay,"%20",statmonth,"%20",Year,")/(",endday,"%20",endMonth,"%20",Year,")/RANGEEDGES/Y/(50N)/(50S)/RANGEEDGES/P/(1000)/VALUES/%5BT%5Daverage/data.nc")
# 
# download.file(link_HR,mode = "wb",paste("Data/",Data_source,"/",Name,".nc",sep = ""))
# 
# Data<-tidync(paste("Data/",Data_source,"/",Name,".nc",sep=""))%>%hyper_tibble(na.rm = F)
# rio::export(Data,paste("Data/",Data_source,"/CSV/",Name,".csv",sep=""))
# 
# 
# Raster_file<-rasterFromXYZ(Data[c("X","Y","v")])
# Raster_file_1=disaggregate(Raster_file,8,method='bilinear')
# rr = raster::mask(Raster_file_1 ,Africa)
# Data <- as.data.frame(rasterToPoints(rr ))
# mybreaks <- c(-10,-5,0,5,10,15,Inf)
# #Function to return the desired number of colors
# mycolors<- function(x) {
#   colors<-colorRampPalette(c("#8cb02c","#37fdf8","blue","#89522a","black","red"))(6)
#   colors[1:x]
# }
# #Function to create labels for legend
# breaklabel <- function(x){
#   labels<- as.character(c(-10,-5,0,5,10,15))
#   labels[1:x]
# }
# #############
# Title<-toupper(paste("Vent Méridien hebdo FROM ",Month,"\nData Source: ",Data_source,sep=""))
# l<-ggplot()+geom_contour_filled(data=Data, aes(x,y,z =v),breaks= mybreaks, show.legend = TRUE) +
#   scale_fill_manual(palette=mycolors, values=breaklabel(6), name="", drop=FALSE, guide = guide_legend(reverse = T))+theme_bw()
# last<-l+geom_polygon(data = Africa, aes(x = long,y = lat, group = group), fill = NA,color = "black",size = 1.1)+ theme(legend.position = c(.04, .04),legend.justification = c("left", "bottom"),legend.box.just = "right",legend.margin = margin(6, 6, 6, 6),legend.text = element_text(size=20),plot.title = element_text(hjust = 0.5,size=25,face = "bold"),axis.text.x = element_text(size=15,face = "bold"),axis.text.y = element_text(size=15,face = "bold"))
# #last<-last+  annotation_custom(Im, xmin = MaxLon-5, xmax = MaxLon, ymin =MaxLat-5, ymax = MaxLat) +coord_cartesian(clip = "off")
# last<-last+metR::scale_x_longitude(limits = c(MinLon,MaxLon),breaks =seq(MinLon,MaxLon,5))+scale_y_latitude(limits = c(MinLat,MaxLat),breaks =seq(MinLat,MaxLat,5))
# last<-last+labs(title = Title,x="",y="")
# jpeg(filename = paste("Products/",Name,".jpeg",sep=""),
#      width = 14,
#      height =14,
#      units = "in",
#      res=300)
# print(last)
# dev.off()
# 
# 
# 
# 
# 
# 
# #Humidité Relative hebdo
# 
# 
# var="Humidite_Relative_hebdo"
# Month="May"
# statDay=29
# statmonth="May"
# endday=5
# endMonth="Jun"
# Year="2022"
# Name=paste(Month,"_",Year,"_",var,sep = "")
# link_HR=paste("http://iridl.ldeo.columbia.edu/expert/expert/SOURCES/.NOAA/.NCEP-NCAR/.CDAS-1/.DAILY/.Intrinsic/.PressureLevel/.rhum/T/(",statDay,"%20",statmonth,"%20",Year,")/(",endday,"%20",endMonth,"%20",Year,")/RANGEEDGES/Y/(50N)/(50S)/RANGEEDGES/P/(1000)/VALUES/X/(40W)/(72E)/RANGEEDGES/%5BT%5Daverage/data.nc")
# 
# download.file(link_HR,mode = "wb",paste("Data/",Data_source,"/",Name,".nc",sep = ""))
# 
# Data<-tidync(paste("Data/",Data_source,"/",Name,".nc",sep=""))%>%hyper_tibble(na.rm = F)
# rio::export(Data,paste("Data/",Data_source,"/CSV/",Name,".csv",sep=""))
# 
# 
# Raster_file<-rasterFromXYZ(Data[c("X","Y","rhum")])
# Raster_file_1=disaggregate(Raster_file,8,method='bilinear')
# rr = raster::mask(Raster_file_1 ,Africa)
# Data <- as.data.frame(rasterToPoints(rr ))
# mybreaks <- c(0,20,40,60,80,100,Inf)
# #Function to return the desired number of colors
# mycolors<- function(x) {
#   colors<-colorRampPalette(c("#8cb02c","#37fdf8","blue","#89522a","black","red"))(6)
#   colors[1:x]
# }
# #Function to create labels for legend
# breaklabel <- function(x){
#   labels<- as.character(c(0,20,40,60,80,100))
#   labels[1:x]
# }
# #############
# Title<-toupper(paste("Humidité Relative hebdo FROM ",Month,"\nData Source: ",Data_source,sep=""))
# l<-ggplot()+geom_contour_filled(data=Data, aes(x,y,z =rhum),breaks= mybreaks, show.legend = TRUE) +
#   scale_fill_manual(palette=mycolors, values=breaklabel(6), name="", drop=FALSE, guide = guide_legend(reverse = T))+theme_bw()
# last<-l+geom_polygon(data = Africa, aes(x = long,y = lat, group = group), fill = NA,color = "black",size = 1.1)+ theme(legend.position = c(.04, .04),legend.justification = c("left", "bottom"),legend.box.just = "right",legend.margin = margin(6, 6, 6, 6),legend.text = element_text(size=20),plot.title = element_text(hjust = 0.5,size=25,face = "bold"),axis.text.x = element_text(size=15,face = "bold"),axis.text.y = element_text(size=15,face = "bold"))
# #last<-last+  annotation_custom(Im, xmin = MaxLon-5, xmax = MaxLon, ymin =MaxLat-5, ymax = MaxLat) +coord_cartesian(clip = "off")
# last<-last+metR::scale_x_longitude(limits = c(MinLon,MaxLon),breaks =seq(MinLon,MaxLon,5))+scale_y_latitude(limits = c(MinLat,MaxLat),breaks =seq(MinLat,MaxLat,5))
# last<-last+labs(title = Title,x="",y="")
# jpeg(filename = paste("Products/",Name,".jpeg",sep=""),
#      width = 14,
#      height =14,
#      units = "in",
#      res=300)
# print(last)
# dev.off()


