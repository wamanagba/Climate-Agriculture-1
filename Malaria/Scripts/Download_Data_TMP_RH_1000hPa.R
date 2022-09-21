#https://nomads.ncep.noaa.gov/cgi-bin/filter_gefs_atmos_0p50a.pl?file=geavg.t00z.pgrb2a.0p50.f168&lev_1000_mb=on&var_RH=on&var_TMP=on&leftlon=-25&rightlon=55&toplat=40&bottomlat=-40&dir=%2Fgefs.20220317%2F00%2Fatmos%2Fpgrb2ap5
#https://nomads.ncep.noaa.gov/cgi-bin/filter_gensbc.pl?file=ge10pt.t00z.pgrb2a.0p50_bcf006&lev_1000_mb=on&var_RH=on&var_TMP=on&subregion=&leftlon=-25&rightlon=55&toplat=40&bottomlat=-40&dir=%2Fgefs.20220321%2F00%2Fpgrb2ap5_bc
library(rio)
library(dplyr)
library(ncdf4)
library(mondate)
rm(list=ls())
options(download.file.method = 'wget')
options(timeout=600)
getOption('timeout')
setwd("E:/Climate-Agriculture/Malaria/")#give the date of the forecast
#Week 1
Time<-seq(24,168,6)
Step=data.frame(Time=Time)

j=1
i=1
for (i in Time) {
  if(nchar(i)==1){
    Step$Time[j]=as.character(paste("00",i,sep=""))
    j=j+1
  }
  if(nchar(i)==2){
    Step$Time[j]=as.character(paste("0",i,sep=""))
    j=j+1
  }
  
}

Date=gsub("-","",Sys.Date()-2)
dir.create(paste("Data/",Date,sep=""),recursive = T,showWarnings = F)
i="012"
for (i in Step$Time) {
  
  system(paste("wget -O Data/",Date,"/GEFS_Avg_",Date,"_",i,".grib2 ","\"https://nomads.ncep.noaa.gov/cgi-bin/filter_gefs_atmos_0p50a.pl?file=geavg.t00z.pgrb2a.0p50.f",i,"&lev_1000_mb=on&var_RH=on&var_TMP=on&subregion=&leftlon=-25&rightlon=55&toplat=40&bottomlat=-40&dir=%2Fgefs.",Date,"%2F00%2Fatmos%2Fpgrb2ap5\"",sep=""))
  #system(paste("wget -O Data/",Date,"/GEFS_Avg_",Date,"_",i,".grib2 ","\"https://nomads.ncep.noaa.gov/cgi-bin/filter_gensbc.pl?file=geavg.t00z.pgrb2a.0p50_bcf",i,"&lev_1000_mb=on&var_RH=on&subregion=&leftlon=-25&rightlon=55&toplat=40&bottomlat=-40&dir=%2Fgefs.",Date,"%2F00%2Fpgrb2ap5_bc\"",sep=""))
  k=paste("GEFS_Avg_",Date,"_",i,".grib2",sep="")
  system(paste("cdo -f nc copy Data/",Date,"/",k, " Data/",Date,"/",substr(k,0,nchar(k)-6),".nc",sep=""))
  dir.create(paste("Data/",Date,"/","ncdf/",sep=""),recursive = T,showWarnings = F)
  system(paste("mv Data/",Date,"/*.nc Data/",Date,"/","ncdf/",sep=""))
}
system(paste("cdo -f nc2 mergetime Data/",Date,"/ncdf/*.nc Data/Week_",1,"_",Date,".nc",sep=""))
system(paste("cdo -timmean Data/Week_",1,"_",Date,".nc", " Data/Weekly_Mean_1_",Date,".nc",sep=""))
system(paste("rm -r Data/",Date,"/*",sep = ""))


#Week 2
Time<-seq(192,360,6)
Step=data.frame(Time=Time)

j=1
i=1
for (i in Time) {
  if(nchar(i)==1){
    Step$Time[j]=as.character(paste("00",i,sep=""))
    j=j+1
  }
  if(nchar(i)==2){
    Step$Time[j]=as.character(paste("0",i,sep=""))
    j=j+1
  }
  
}

Date=gsub("-","",Sys.Date()-2)
dir.create(paste("Data/GEFS/",Date,sep=""),recursive = T,showWarnings = F)

for (i in Step$Time){
  
  system(paste("wget -O Data/GEFS/",Date,"/GEFS_Avg_",Date,"_",i,".grib2 ","\"https://nomads.ncep.noaa.gov/cgi-bin/filter_gefs_atmos_0p50a.pl?file=geavg.t00z.pgrb2a.0p50.f",i,"&lev_1000_mb=on&var_RH=on&var_TMP=on&subregion=&leftlon=-25&rightlon=55&toplat=40&bottomlat=-40&dir=%2Fgefs.",Date,"%2F00%2Fatmos%2Fpgrb2ap5\"",sep=""))
  #system(paste("wget -O Data/",Date,"/GEFS_Avg_",Date,"_",i,".grib2 ","\"https://nomads.ncep.noaa.gov/cgi-bin/filter_gensbc.pl?file=ge10pt.t00z.pgrb2a.0p50_bcf",i,"&lev_1000_mb=on&var_RH=on&var_TMP=on&subregion=&leftlon=-25&rightlon=55&toplat=40&bottomlat=-40&dir=%2Fgefs.",Date,"%2F00%2Fpgrb2ap5_bc\"",sep=""))
  
  k=paste("GEFS_Avg_",Date,"_",i,".grib2",sep="")
  system(paste("cdo -f nc copy Data/GEFS/",Date,"/",k, " Data/GEFS/",Date,"/",substr(k,0,nchar(k)-6),".nc",sep=""))
  dir.create(paste("Data/GEFS/",Date,"/","ncdf/",sep=""),recursive = T,showWarnings = F)
  system(paste("mv Data/GEFS/",Date,"/*.nc Data/GEFS/",Date,"/","ncdf/",sep=""))
  
}
system(paste("cdo -f nc2 mergetime Data/GEFS/",Date,"/ncdf/*.nc Data/GEFS/Week_",2,"_",Date,".nc",sep=""))
system(paste("cdo -timmean Data/GEFS/Week_",2,"_",Date,".nc", " Data/GEFS/Weekly_Mean_2_",Date,".nc",sep=""))
#system(paste("rm -r Data/",Date,"/*",sep = ""))



##### Week1#######################33

Data<-tidync(paste("Data/Weekly_Mean_1_",Date,".nc",sep=""))%>%hyper_tibble(na.rm = F)
Data$t=Data$t-273.15
names(Data)[3]="X"
names(Data)[4]="Y"
Data_HR <- Data[,-(5:6)]    



Day1=format(mondate(Sys.Date()-2),"%d")
Day2=format(mondate(Sys.Date()+5),"%d")
Month1=format(mondate(Sys.Date()-2),"%b")
Month2=format(mondate(Sys.Date()+5),"%b")
Year=format(mondate(Sys.Date()+5),"%Y")

periode=paste("From ",Day1," ",Month1," to ",Day2," ",Month2," ",Year,sep = "")


# Humidite Relative mensuelle
# var="HR"
# # Month="Jul"
# Year="2022"
Name=periode
Data_source="GEFS"
# download.file(paste("http://iridl.ldeo.columbia.edu/expert/expert/SOURCES/.NOAA/.NCEP-NCAR/.CDAS-1/.MONTHLY/.Intrinsic/.PressureLevel/.r/P/(1000)/VALUES/Y/-40/2/40/GRID/X/-20/2/55/GRID/T/(",Month,"%20",Year,")/VALUES/data.nc",sep = ""),mode = "wb",paste("Data/",Data_source,"/",Name,".nc",sep = ""))
# Data<-tidync(paste("Data/",Data_source,"/",Name,".nc",sep=""))%>%hyper_tibble(na.rm = F)
# rio::export(Data,paste("Data/",Data_source,"/CSV/",Name,".csv",sep=""))
Data=Data_HR

MinLon=-25
MaxLon=55
MinLat=-40
MaxLat=40
Africa<-readOGR("~/Desktop/ACMAD_Git/SHP_AFRIQUE/Afrique_frontier_news.shp") 


#################### Relative Humidity  ######################


Raster_file<-rasterFromXYZ(Data[c("X","Y","r")])
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
Title<-toupper(paste("Relative Humidity from ",Name,"-","\nData Source: ",Data_source,sep=""))
l<-ggplot()+geom_contour_filled(data=Data, aes(x,y,z =r),breaks= mybreaks, show.legend = TRUE) +
  scale_fill_manual(palette=mycolors, values=breaklabel(6), name="Relative Humidity(%)", drop=FALSE, guide = guide_legend(reverse = T))+theme_bw()
last<-l+geom_polygon(data = Africa, aes(x = long,y = lat, group = group), fill = NA,color = "black",size = 1.1)+ theme(legend.position = c(.04, .04),legend.justification = c("left", "bottom"),legend.box.just = "right",legend.margin = margin(6, 6, 6, 6),legend.text = element_text(size=20),plot.title = element_text(hjust = 0.5,size=25,face = "bold"),axis.text.x = element_text(size=15,face = "bold"),axis.text.y = element_text(size=15,face = "bold"))
#last<-last+  annotation_custom(Im, xmin = MaxLon-5, xmax = MaxLon, ymin =MaxLat-5, ymax = MaxLat) +coord_cartesian(clip = "off")
last<-last+metR::scale_x_longitude(limits = c(MinLon,MaxLon),breaks =seq(MinLon,MaxLon,5))+scale_y_latitude(limits = c(MinLat,MaxLat),breaks =seq(MinLat,MaxLat,5))
last<-last+labs(title = Title,x="",y="")
dir.create(paste("Products/",Name,"/",sep=""),recursive = T,showWarnings = F)
jpeg(filename = paste("Products/",Name,"/",Year,"HR.jpeg",sep=""),
     width = 12,
     height =14,
     units = "in",
     res=300)
print(last)
dev.off()







#################### temperature  ######################

Data=Data_HR
Raster_file<-rasterFromXYZ(Data[c("X","Y","t")])
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
Title<-toupper(paste("Surface temperature ",Name,"-","\nData Source: ",Data_source,sep=""))
l<-ggplot()+geom_contour_filled(data=Data, aes(x,y,z =t),breaks= mybreaks, show.legend = TRUE) +
  scale_fill_manual(palette=mycolors, values=breaklabel(9), name="Temperature(°C)", drop=FALSE, guide = guide_legend(reverse = T))+theme_bw()
last<-l+geom_polygon(data = Africa, aes(x = long,y = lat, group = group), fill = NA,color = "black",size = 1.1)+ theme(legend.position = c(.04, .04),legend.justification = c("left", "bottom"),legend.box.just = "right",legend.margin = margin(6, 6, 6, 6),legend.text = element_text(size=20),plot.title = element_text(hjust = 0.5,size=25,face = "bold"),axis.text.x = element_text(size=15,face = "bold"),axis.text.y = element_text(size=15,face = "bold"))
#last<-last+  annotation_custom(Im, xmin = MaxLon-5, xmax = MaxLon, ymin =MaxLat-5, ymax = MaxLat) +coord_cartesian(clip = "off")
last<-last+metR::scale_x_longitude(limits = c(MinLon,MaxLon),breaks =seq(MinLon,MaxLon,5))+scale_y_latitude(limits = c(MinLat,MaxLat),breaks =seq(MinLat,MaxLat,5))
last<-last+labs(title = Title,x="",y="")
#dir.create(paste("Products/malaria/",sep=""),recursive = T,showWarnings = F)
jpeg(filename = paste("Products/",Name,"/",Year,"Temp.jpeg",sep=""),
     width = 12,
     height =14,
     units = "in",
     res=300)
print(last)
dev.off()




#################### Malaria maps##################

### Mask for the temperature #####

DATA=Data_HR

DATA$Vigilance=4
DATA$mask_rh<-NA
DATA$mask_tmp<-NA

# masque RH

DATA$mask_rh<-ifelse(DATA$r>=80,1,DATA$mask_rh)
DATA$mask_rh<-ifelse(DATA$r>=70 & DATA$r<80,2,DATA$mask_rh)
DATA$mask_rh<-ifelse(DATA$r>=60 & DATA$r<=70,3,DATA$mask_rh)
DATA$mask_rh<-ifelse(DATA$r<60,4,DATA$mask_rh)

#masque temp
DATA$mask_tmp<-ifelse(DATA$t>=25 & DATA$t<=32,1,DATA$mask_tmp)

DATA$mask_tmp<-ifelse(DATA$t>=20 & DATA$t<25,2,DATA$mask_tmp)

DATA$mask_tmp<-ifelse(DATA$t>=18 & DATA$t<20,3,DATA$mask_tmp)

DATA$mask_tmp<-ifelse(DATA$t<18,4,DATA$mask_tmp)

DATA$mask_tmp<-ifelse(DATA$t>32,4,DATA$mask_tmp)


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



# DATA$cond1=ifelse((DATA$t>=25 & DATA$t<=32)&(DATA$r>80),4,0)
# DATA$cond2=ifelse((DATA$t>=20 & DATA$t<25)&(DATA$r>=70 & DATA$r<=80),3,0)
# DATA$cond3=ifelse((DATA$t>=18 & DATA$t<20)&(DATA$r>=60 & DATA$r<70),2,0)
# DATA$cond4=ifelse((DATA$t<18)&(DATA$r<60),1,0)
# DATA$cond5=DATA$cond1+DATA$cond2+DATA$cond3+DATA$cond4

var="Malaria"
# Month="Jun"
Year="2022" 
# Name=paste(Month,"_",Year,"_",var,sep = "")
Raster_file<-rasterFromXYZ(DATA[c("X","Y","Vigilance")])
Raster_file_1=disaggregate(Raster_file,20,method='bilinear')
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
Title<-toupper(paste("Malaria Vigilance ",Name, "\nData Source: ",Data_source,sep=""))
l<-ggplot()+geom_contour_filled(data=DATA, aes(x,y,z =Vigilance),breaks= mybreaks, show.legend = TRUE) +
  scale_fill_manual(palette=mycolors, values=breaklabel(4), name="", drop=FALSE, guide = guide_legend(reverse = T))+theme_bw()
last<-l+geom_polygon(data = Africa, aes(x = long,y = lat, group = group), fill = NA,color = "black",size = 1.1)+ theme(legend.position = c(.04, .04),legend.justification = c("left", "bottom"),legend.box.just = "right",legend.margin = margin(6, 6, 6, 6),legend.text = element_text(size=20),plot.title = element_text(hjust = 0.5,size=25,face = "bold"),axis.text.x = element_text(size=15,face = "bold"),axis.text.y = element_text(size=15,face = "bold"))
#last<-last+  annotation_custom(Im, xmin = MaxLon-5, xmax = MaxLon, ymin =MaxLat-5, ymax = MaxLat) +coord_cartesian(clip = "off")
last<-last+metR::scale_x_longitude(limits = c(MinLon,MaxLon),breaks =seq(MinLon,MaxLon,5))+scale_y_latitude(limits = c(MinLat,MaxLat),breaks =seq(MinLat,MaxLat,5))
last<-last+labs(title = Title,x="",y="")
#dir.create(paste("Products/malaria/",sep=""),recursive = T,showWarnings = F)
jpeg(filename = paste("Products/",Name,"/",Year,"Vigilance.jpeg",sep=""),
     width = 10,
     height =12,
     units = "in",
     res=300)
print(last)
dev.off()









##### Week 2 #######################33

Data<-tidync(paste("Data/GEFS/Weekly_Mean_2_",Date,".nc",sep=""))%>%hyper_tibble(na.rm = F)
Data$t=Data$t-273.15
names(Data)[3]="X"
names(Data)[4]="Y"
Data_HR <- Data[,-(5:6)]    



Day1=format(mondate(Sys.Date()+6),"%d")
Day2=format(mondate(Sys.Date()+14),"%d")
Month1=format(mondate(Sys.Date()+6),"%b")
Month2=format(mondate(Sys.Date()+14),"%b")
Year=format(mondate(Sys.Date()),"%Y")

periode=paste("From ",Day1," ",Month1," to ",Day2," ",Month2," ",Year,sep = "")


# Humidite Relative mensuelle
# var="HR"
# # Month="Jul"
# Year="2022"
Name=periode
Data_source="GEFS"
# download.file(paste("http://iridl.ldeo.columbia.edu/expert/expert/SOURCES/.NOAA/.NCEP-NCAR/.CDAS-1/.MONTHLY/.Intrinsic/.PressureLevel/.r/P/(1000)/VALUES/Y/-40/2/40/GRID/X/-20/2/55/GRID/T/(",Month,"%20",Year,")/VALUES/data.nc",sep = ""),mode = "wb",paste("Data/",Data_source,"/",Name,".nc",sep = ""))
# Data<-tidync(paste("Data/",Data_source,"/",Name,".nc",sep=""))%>%hyper_tibble(na.rm = F)
# rio::export(Data,paste("Data/",Data_source,"/CSV/",Name,".csv",sep=""))
Data=Data_HR

MinLon=-25
MaxLon=55
MinLat=-40
MaxLat=40
Africa<-readOGR("~/Desktop/ACMAD_Git/SHP_AFRIQUE/Afrique_frontier_news.shp") 


#################### Relative Humidity  ######################


Raster_file<-rasterFromXYZ(Data[c("X","Y","r")])
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
Title<-toupper(paste("Relative Humidity from ",Name,"-","\nData Source: ",Data_source,sep=""))
l<-ggplot()+geom_contour_filled(data=Data, aes(x,y,z =r),breaks= mybreaks, show.legend = TRUE) +
  scale_fill_manual(palette=mycolors, values=breaklabel(6), name="Relative Humidity(%)", drop=FALSE, guide = guide_legend(reverse = T))+theme_bw()
last<-l+geom_polygon(data = Africa, aes(x = long,y = lat, group = group), fill = NA,color = "black",size = 1.1)+ theme(legend.position = c(.04, .04),legend.justification = c("left", "bottom"),legend.box.just = "right",legend.margin = margin(6, 6, 6, 6),legend.text = element_text(size=20),plot.title = element_text(hjust = 0.5,size=25,face = "bold"),axis.text.x = element_text(size=15,face = "bold"),axis.text.y = element_text(size=15,face = "bold"))
#last<-last+  annotation_custom(Im, xmin = MaxLon-5, xmax = MaxLon, ymin =MaxLat-5, ymax = MaxLat) +coord_cartesian(clip = "off")
last<-last+metR::scale_x_longitude(limits = c(MinLon,MaxLon),breaks =seq(MinLon,MaxLon,5))+scale_y_latitude(limits = c(MinLat,MaxLat),breaks =seq(MinLat,MaxLat,5))
last<-last+labs(title = Title,x="",y="")
dir.create(paste("Products/",Name,"/",sep=""),recursive = T,showWarnings = F)
jpeg(filename = paste("Products/",Name,"/",Year,"HR.jpeg",sep=""),
     width = 12,
     height =14,
     units = "in",
     res=300)
print(last)
dev.off()







#################### temperature  ######################

Data=Data_HR
Raster_file<-rasterFromXYZ(Data[c("X","Y","t")])
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
Title<-toupper(paste("Surface temperature ",Name,"-","\nData Source: ",Data_source,sep=""))
l<-ggplot()+geom_contour_filled(data=Data, aes(x,y,z =t),breaks= mybreaks, show.legend = TRUE) +
  scale_fill_manual(palette=mycolors, values=breaklabel(9), name="Temperature(°C)", drop=FALSE, guide = guide_legend(reverse = T))+theme_bw()
last<-l+geom_polygon(data = Africa, aes(x = long,y = lat, group = group), fill = NA,color = "black",size = 1.1)+ theme(legend.position = c(.04, .04),legend.justification = c("left", "bottom"),legend.box.just = "right",legend.margin = margin(6, 6, 6, 6),legend.text = element_text(size=20),plot.title = element_text(hjust = 0.5,size=25,face = "bold"),axis.text.x = element_text(size=15,face = "bold"),axis.text.y = element_text(size=15,face = "bold"))
#last<-last+  annotation_custom(Im, xmin = MaxLon-5, xmax = MaxLon, ymin =MaxLat-5, ymax = MaxLat) +coord_cartesian(clip = "off")
last<-last+metR::scale_x_longitude(limits = c(MinLon,MaxLon),breaks =seq(MinLon,MaxLon,5))+scale_y_latitude(limits = c(MinLat,MaxLat),breaks =seq(MinLat,MaxLat,5))
last<-last+labs(title = Title,x="",y="")
#dir.create(paste("Products/malaria/",sep=""),recursive = T,showWarnings = F)
jpeg(filename = paste("Products/",Name,"/",Year,"Temp.jpeg",sep=""),
     width = 12,
     height =14,
     units = "in",
     res=300)
print(last)
dev.off()




#################### Malaria maps##################

### Mask for the temperature #####

DATA=Data_HR

DATA$Vigilance=4
DATA$mask_rh<-NA
DATA$mask_tmp<-NA

# masque RH

DATA$mask_rh<-ifelse(DATA$r>=80,1,DATA$mask_rh)
DATA$mask_rh<-ifelse(DATA$r>=70 & DATA$r<80,2,DATA$mask_rh)
DATA$mask_rh<-ifelse(DATA$r>=60 & DATA$r<=70,3,DATA$mask_rh)
DATA$mask_rh<-ifelse(DATA$r<60,4,DATA$mask_rh)

#masque temp
DATA$mask_tmp<-ifelse(DATA$t>=25 & DATA$t<=32,1,DATA$mask_tmp)

DATA$mask_tmp<-ifelse(DATA$t>=20 & DATA$t<25,2,DATA$mask_tmp)

DATA$mask_tmp<-ifelse(DATA$t>=18 & DATA$t<20,3,DATA$mask_tmp)

DATA$mask_tmp<-ifelse(DATA$t<18,4,DATA$mask_tmp)

DATA$mask_tmp<-ifelse(DATA$t>32,4,DATA$mask_tmp)


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



# DATA$cond1=ifelse((DATA$t>=25 & DATA$t<=32)&(DATA$r>80),4,0)
# DATA$cond2=ifelse((DATA$t>=20 & DATA$t<25)&(DATA$r>=70 & DATA$r<=80),3,0)
# DATA$cond3=ifelse((DATA$t>=18 & DATA$t<20)&(DATA$r>=60 & DATA$r<70),2,0)
# DATA$cond4=ifelse((DATA$t<18)&(DATA$r<60),1,0)
# DATA$cond5=DATA$cond1+DATA$cond2+DATA$cond3+DATA$cond4

var="Malaria"
# Month="Jun"
Year="2022" 
# Name=paste(Month,"_",Year,"_",var,sep = "")
Raster_file<-rasterFromXYZ(DATA[c("X","Y","Vigilance")])
Raster_file_1=disaggregate(Raster_file,20,method='bilinear')
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
Title<-toupper(paste("Malaria Vigilance ",Name, "\nData Source: ",Data_source,sep=""))
l<-ggplot()+geom_contour_filled(data=DATA, aes(x,y,z =Vigilance),breaks= mybreaks, show.legend = TRUE) +
  scale_fill_manual(palette=mycolors, values=breaklabel(4), name="", drop=FALSE, guide = guide_legend(reverse = T))+theme_bw()
last<-l+geom_polygon(data = Africa, aes(x = long,y = lat, group = group), fill = NA,color = "black",size = 1.1)+ theme(legend.position = c(.04, .04),legend.justification = c("left", "bottom"),legend.box.just = "right",legend.margin = margin(6, 6, 6, 6),legend.text = element_text(size=20),plot.title = element_text(hjust = 0.5,size=25,face = "bold"),axis.text.x = element_text(size=15,face = "bold"),axis.text.y = element_text(size=15,face = "bold"))
#last<-last+  annotation_custom(Im, xmin = MaxLon-5, xmax = MaxLon, ymin =MaxLat-5, ymax = MaxLat) +coord_cartesian(clip = "off")
last<-last+metR::scale_x_longitude(limits = c(MinLon,MaxLon),breaks =seq(MinLon,MaxLon,5))+scale_y_latitude(limits = c(MinLat,MaxLat),breaks =seq(MinLat,MaxLat,5))
last<-last+labs(title = Title,x="",y="")
#dir.create(paste("Products/malaria/",sep=""),recursive = T,showWarnings = F)
jpeg(filename = paste("Products/",Name,"/",Year,"Vigilance.jpeg",sep=""),
     width = 10,
     height =12,
     units = "in",
     res=300)
print(last)
dev.off()
