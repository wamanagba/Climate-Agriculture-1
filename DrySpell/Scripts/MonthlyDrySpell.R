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
library(mondate)
library(tidync)
options(download.file.extra = '--no-check-certificate')
rm(list=ls())
#dir.create("Results",recursive = T,showWarnings = F)
#Shape file
setwd("E:/Climate-Agriculture-1/DrySpell/")
source("Scripts/Last_spell.R")
source("Scripts/Dry_spell_Funct.R")
Africa<-readOGR("E:/ACMAD_Git/SHP_AFRIQUE/Afrique_frontier_news.shp") 

Start_Month="Jul"
End_Month="Sep"
Season_Name='JAS'

Data_Source="CPC-UNIFIED"
MinLon=-25
MaxLon=55
MinLat=-40
MaxLat=40

month='Aug'
Month_name="August"
dir.create(paste("Data/Monthly_dry/",Month_name,"/",sep=""),recursive = T,showWarnings = F)

Day=format(mondate(Sys.Date()-2), "%d")
Month=format(mondate(Sys.Date()), "%b")


#Month_name=format(mondate(Sys.Date()), "%B")
Year=format(mondate(Sys.Date()), "%Y")

dir.create(paste("Data/",Data_Source,sep=""),recursive = T,showWarnings = F)

Link_CPC_UNIFIED="https://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP/.CPC/.UNIFIED_PRCP/.GAUGE_BASED/.GLOBAL/.v1p0/.extREALTIME/.rain/"

#download.file(paste(Link_CPC_UNIFIED,"T/(1%20Jan%20",Year,")/(%20",Day,"%20",Month,"%20",Year,")/RANGEEDGES/Y/",MinLat,"/0.5/",MaxLat,"/GRID/X/",MinLon,"/0.5/",MaxLon,"/GRID/0.0/setmissing_value/data.nc",sep=""),mode="wb",paste("Data/",Data_Source,"/",Year,".nc",sep=""))
#download.file(paste("https://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP/.CPC/.UNIFIED_PRCP/.GAUGE_BASED/.GLOBAL/.v1p0/.extREALTIME/.rain/X/%2825E%29%2855W%29RANGEEDGES/T/%280000%201%20Jan%202022%29%280000%20",Day,"%20",Month,"%202022%29RANGEEDGES/Y/%2840S%29%2840N%29RANGEEDGES/data.nc"),mode="wb",paste("Data/",Data_Source,"/",Year,".nc",sep=""))
download.file(paste("https://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP/.CPC/.UNIFIED_PRCP/.GAUGE_BASED/.GLOBAL/.v1p0/.extREALTIME/.rain/T/(0000%201%20Jan%202022)/(0000%20",Day,"%20",Month,"%20",Year,")/RANGEEDGES/Y/-40/0.5/40/GRID/X/-20/0.5/55/GRID/data.nc"),mode="wb",paste("Data/",Data_Source,"/",Year,".nc",sep=""))


#i=Year
#download.file(paste(Link_CPC_UNIFIED,"T/(1%20Jan%20",i,")/(%2031%20Dec%20",i,")/RANGEEDGES/Y/",MinLat,"/0.5/",MaxLat,"/GRID/X/",MinLon,"/0.5/",MaxLon,"/GRID/0.0/setmissing_value/data.nc",sep=""),mode="wb",paste("Data/",Data_Source,"/",Year,".nc",sep=""))






Data_NC<-nc_open(paste("Data/",Data_Source,"/",Year,".nc",sep=""))
Data<-tidync(paste("Data/",Data_Source,"/",Year,".nc",sep=""))%>%hyper_tibble(na.rm = F)
#Date=seq(as.Date(paste(k,"-01-01",sep="")),as.Date(paste(k,"-12-31",sep="")),by="days")
Date=seq(as.Date(paste(Year,"-01-01",sep="")),as.Date(mondate(Sys.Date()-2)),by="days")
X<-length(ncvar_get(Data_NC,"X"))
Y<-length(ncvar_get(Data_NC,"Y"))
Date_All=sort(rep(Date,X*Y),decreasing = F)

Data$T<-Date_All
Data$Months=format(Data$T,"%b")
Data=filter(Data,Months==month)
Data$rain[is.na(Data$rain)]=0

Data_Dry_Speel_last<-Data%>%
   group_by(X,Y)%>%
   summarise(Spell=Function_speel(rain))

Data_Dry_Speel10<-Data%>%
  group_by(X,Y)%>%
  summarise(Spell=CDD_function(rain,2.5,Nb=10))

Data_Dry_SpeelMax<-Data%>%
  group_by(X,Y)%>%
  summarise(Spell=CDD_function_Max(rain,2.5,Nb=10))

Data_Dry_SpeelMean<-Data%>%
  group_by(X,Y)%>%
  summarise(Spell=CDD_function_Mean(rain,2.5,Nb=10))

# season='JAS'
# Mask=rio::import(paste("E:/ACMAD_Git/Data/CPC-UNIFIED/Mask/mask_ ",season," .csv",sep = ""))
#Mask File

dir.create("Mask")
download.file(paste("http://154.66.220.45:8080/thredds/fileServer/ACMAD/CDD/climatedataservice/Seasonal_Climate_Zones/Masked/CAMS_OPI/1981-2010/Climatology_",Start_Month,"_",End_Month,"_1981_2010_CAMS_Masked.nc",sep=""),mode = "wb",paste("Mask/",Season_Name,".nc",sep=""))

Mask<-tidync::tidync(paste("Mask/",Season_Name,".nc",sep=""))%>%hyper_tibble()
names(Mask)[2]="X"
names(Mask)[3]="Y"

DATA_last=merge(Data_Dry_Speel_last,Mask,by=c("X","Y"))
DATA_last$Spell=ifelse(DATA_last$mask==-1,-5,DATA_last$Spell)
DATA_L=filter(DATA_last,mask==1)
dir.create("Data/Monthly_dry/",recursive = T,showWarnings = F)
rio::export(DATA_last,paste("Data/Monthly_dry/",Month_name,"Spellast.csv",sep=""))  


DATA_F10=merge(Data_Dry_Speel10,Mask,by=c("X","Y"))
DATA_F10$Spell=ifelse(DATA_F10$mask==-1,-5,DATA_F10$Spell)
DATA_10=filter(DATA_F10,mask==1)
#dir.create("Data/Monthly_dry/",recursive = T,showWarnings = F)
rio::export(DATA_F10,paste("Data/Monthly_dry/",Month_name,"Spell10.csv",sep=""))  

DATA_FMax=merge(Data_Dry_SpeelMax,Mask,by=c("X","Y"))
DATA_FMax$Spell=ifelse(DATA_FMax$mask==-1,-5,DATA_FMax$Spell)
DATA_FMx=filter(DATA_FMax,mask==1)
rio::export(DATA_FMax,paste("Data/Monthly_dry/",Month_name,"SpellMax2.csv",sep=""))  


DATA_FMean=merge(Data_Dry_SpeelMean,Mask,by=c("X","Y"))
DATA_FMean$Spell=ifelse(DATA_FMean$mask==-1,-5,DATA_FMean$Spell)
DATA_FMe=filter(DATA_FMean,mask==1)
rio::export(DATA_FMean,paste("Data/Monthly_dry/",Month_name,"SpellMean.csv",sep=""))  


#############################################
#############################################
#############################################

d_DATA_F10=DATA_F10
Raster_file<-rasterFromXYZ(DATA_F10[c("X","Y","Spell")])

Raster_file_1=disaggregate(Raster_file,10,method='bilinear')

rr = raster::mask(Raster_file_1 ,Africa)

DATA_F10 <- as.data.frame(rasterToPoints(rr ))
#rio::export(Data,"Data/Annual_Total_Mean_1983_2021_CHIRPS.csv")
mybreaks <- c(-5,0,1,2,3,Inf)

#Function to return the desired number of colors

mycolors<- function(x) {
  colors<-colorRampPalette(c("gray66","#096A09" , "#C2F732", "#CC5500" ,"#A91101"))(5)
  colors[1:x]
}

#Function to create labels for legend

breaklabel <- function(x){
  labels<- as.character(c('No season',0,1,2,3))
  labels[1:x]
}
################################################################################

#Title<-paste("Number of dry spell overs 10 days", "\nRef: 1981-2010","\nData Source: ",Data_Source,"\n Season:",season,sep="")
Title<-toupper(paste("Number of consecutive dry spells over 10 days for ",Month_name," \nData Source: ",Data_Source,sep=""))

#Im<-grid::rasterGrob(png::readPNG("Logos/Acmad_logo_1.png"), interpolate = TRUE)

l<-ggplot()+geom_contour_filled(data=DATA_F10, aes(x,y,z =Spell),breaks= mybreaks, show.legend = TRUE) +
  scale_fill_manual(palette=mycolors, values=breaklabel(5), name="Number of Dry Spell>10 days", drop=FALSE, guide = guide_legend(reverse = T))+theme_bw()

last<-l+geom_polygon(data = Africa, aes(x = long,y = lat, group = group), fill = NA,color = "black",size = 1.1)+ theme(legend.position = c(.04, .04),legend.justification = c("left", "bottom"),legend.box.just = "right",legend.margin = margin(6, 6, 6, 6),legend.text = element_text(size=20),plot.title = element_text(hjust = 0.5,size=25,face = "bold"),axis.text.x = element_text(size=15,face = "bold"),axis.text.y = element_text(size=15,face = "bold"))

#last<-last+  annotation_custom(Im, xmin = MaxLon-5, xmax = MaxLon, ymin =MaxLat-5, ymax = MaxLat) +coord_cartesian(clip = "off")

last<-last+metR::scale_x_longitude(limits = c(MinLon,MaxLon),breaks =seq(MinLon,MaxLon,5))+scale_y_latitude(limits = c(MinLat,MaxLat),breaks =seq(MinLat,MaxLat,5))

last<-last+labs(title = Title,x="ddd",y="xxx")

dir.create(paste("Results/Monthly_dry/",Month_name,sep=""),recursive = T,showWarnings = F)
jpeg(filename = paste("Results/Monthly_dry/",Month_name,"/NumberDrySpell10.jpeg",sep=""),
     width = 13,
     height =15,
     units = "in",
     res=300)
print(last)
dev.off()


################################################################################
################################################################################
################################################################################




d_DATA_last=DATA_last
Raster_file<-rasterFromXYZ(DATA_last[c("X","Y","Spell")])

Raster_file_1=disaggregate(Raster_file,10,method='bilinear')

rr = raster::mask(Raster_file_1 ,Africa)

DATA_last <- as.data.frame(rasterToPoints(rr ))
#rio::export(Data,"Data/Annual_Total_Mean_1983_2021_CHIRPS.csv")
mybreaks <- c(-5,0,1,2,3,Inf)

#Function to return the desired number of colors

mycolors<- function(x) {
  colors<-colorRampPalette(c("gray66","#096A09" , "#C2F732", "#CC5500" ,"#A91101"))(5)
  colors[1:x]
}

#Function to create labels for legend

breaklabel <- function(x){
  labels<- as.character(c("No Season",0,1,2,3))
  labels[1:x]
}
################################################################################

#Title<-paste("Number of dry spell overs 10 days", "\nRef: 1981-2010","\nData Source: ",Data_Source,"\n Season:",season,sep="")
Title<-toupper(paste("Number of last consecutive dry days for ",Month_name," \nData Source: ",Data_Source,sep=""))

#Im<-grid::rasterGrob(png::readPNG("Logos/Acmad_logo_1.png"), interpolate = TRUE)

l<-ggplot()+geom_contour_filled(data=DATA_last, aes(x,y,z =Spell),breaks= mybreaks, show.legend = TRUE) +
  scale_fill_manual(palette=mycolors, values=breaklabel(5), name="current dry spell", drop=FALSE, guide = guide_legend(reverse = T))+theme_bw()

last<-l+geom_polygon(data = Africa, aes(x = long,y = lat, group = group), fill = NA,color = "black",size = 1.1)+ theme(legend.position = c(.04, .04),legend.justification = c("left", "bottom"),legend.box.just = "right",legend.margin = margin(6, 6, 6, 6),legend.text = element_text(size=20),plot.title = element_text(hjust = 0.5,size=25,face = "bold"),axis.text.x = element_text(size=15,face = "bold"),axis.text.y = element_text(size=15,face = "bold"))

#last<-last+  annotation_custom(Im, xmin = MaxLon-5, xmax = MaxLon, ymin =MaxLat-5, ymax = MaxLat) +coord_cartesian(clip = "off")

last<-last+metR::scale_x_longitude(limits = c(MinLon,MaxLon),breaks =seq(MinLon,MaxLon,5))+scale_y_latitude(limits = c(MinLat,MaxLat),breaks =seq(MinLat,MaxLat,5))

last<-last+labs(title = Title,x="ddd",y="xxx")
#dir.create(paste("Products/Number_Rain_Day/Month2_5/Anomaly/",sep=""),recursive = T,showWarnings = F)

jpeg(filename = paste("Results/Monthly_dry/",Month_name,"/NumberDrySpell_last.jpeg",sep=""),
     width = 13,
     height =15,
     units = "in",
     res=300)
print(last)
dev.off()







#############################################
###############################################
###############################################

d_DATA_FMax=DATA_FMax
Raster_file<-rasterFromXYZ(DATA_FMax[c("X","Y","Spell")])

Raster_file_1=disaggregate(Raster_file,20,method='bilinear')

rr = raster::mask(Raster_file_1 ,Africa)

DATA_FMax <- as.data.frame(rasterToPoints(rr ))
#rio::export(Data,"Data/Annual_Total_Mean_1983_2021_CHIRPS.csv")
mybreaks <- c(-5,0,1,2,3,4,5,6,8,9,10,Inf)

#Function to return the desired number of colors

mycolors<- function(x) {
  colors<-colorRampPalette(c("gray66","#096A09" ,"#1E7FCB","#26C4EC","#00FF00", "#C2F732","#766F64","#C8AD7F","#723E64", "#CC5500" ,"#A91101"))(11)
  colors[1:x]
}

#Function to create labels for legend

breaklabel <- function(x){
  labels<- as.character(c("No Season",0,1,2,3,4,5,6,8,9,10))
  labels[1:x]
}
################################################################################

#Title<-paste("Number of dry spell overs 10 days", "\nRef: 1981-2010","\nData Source: ",Data_Source,"\n Season:",season,sep="")
Title<-toupper(paste("Maximunm of Dry Spells for ",Month_name," \nData Source: ",Data_Source,sep=""))

#Im<-grid::rasterGrob(png::readPNG("Logos/Acmad_logo_1.png"), interpolate = TRUE)

l<-ggplot()+geom_contour_filled(data=DATA_FMax, aes(x,y,z =Spell),breaks= mybreaks, show.legend = TRUE) +
  scale_fill_manual(palette=mycolors, values=breaklabel(11), name="Longest Dry spell observed", drop=FALSE, guide = guide_legend(reverse = T))+theme_bw()

last<-l+geom_polygon(data = Africa, aes(x = long,y = lat, group = group), fill = NA,color = "black",size = 1.1)+ theme(legend.position = c(.04, .04),legend.justification = c("left", "bottom"),legend.box.just = "right",legend.margin = margin(6, 6, 6, 6),legend.text = element_text(size=20),plot.title = element_text(hjust = 0.5,size=25,face = "bold"),axis.text.x = element_text(size=15,face = "bold"),axis.text.y = element_text(size=15,face = "bold"))

#last<-last+  annotation_custom(Im, xmin = MaxLon-5, xmax = MaxLon, ymin =MaxLat-5, ymax = MaxLat) +coord_cartesian(clip = "off")

last<-last+metR::scale_x_longitude(limits = c(MinLon,MaxLon),breaks =seq(MinLon,MaxLon,5))+scale_y_latitude(limits = c(MinLat,MaxLat),breaks =seq(MinLat,MaxLat,5))

last<-last+labs(title = Title,x="",y="")

jpeg(filename = paste("Results/Monthly_dry/",Month_name,"/DrySpellMax.jpeg",sep=""),
     width = 13,
     height =15,
     units = "in",
     res=300)
print(last)
dev.off()


################################################################################
################################################################################
################################################################################





#############################################
###############################################
###############################################

d_DATA_FMean=DATA_FMean
Raster_file<-rasterFromXYZ(DATA_FMean[c("X","Y","Spell")])

Raster_file_1=disaggregate(Raster_file,20,method='bilinear')

rr = raster::mask(Raster_file_1 ,Africa)

DATA_FMean <- as.data.frame(rasterToPoints(rr ))
#rio::export(Data,"Data/Annual_Total_Mean_1983_2021_CHIRPS.csv")
mybreaks <- c(-5,0,1,2,3,4,5,6,8,9,10,Inf)

#Function to return the desired number of colors

mycolors<- function(x) {
  colors<-colorRampPalette(c("gray66","#096A09" ,"#1E7FCB","#26C4EC","#00FF00", "#C2F732","#766F64","#C8AD7F","#723E64", "#CC5500" ,"#A91101"))(11)
  colors[1:x]
}

#Function to create labels for legend

breaklabel <- function(x){
  labels<- as.character(c("No Season",0,1,2,3,4,5,6,8,9,10))
  labels[1:x]
}
################################################################################

#Title<-paste("Number of dry spell overs 10 days", "\nRef: 1981-2010","\nData Source: ",Data_Source,"\n Season:",season,sep="")
Title<-toupper(paste("Average of Consecutive dry day observed for ",Month_name," \nData Source: ",Data_Source,sep=""))

#Im<-grid::rasterGrob(png::readPNG("Logos/Acmad_logo_1.png"), interpolate = TRUE)

l<-ggplot()+geom_contour_filled(data=DATA_FMean, aes(x,y,z =Spell),breaks= mybreaks, show.legend = TRUE) +
  scale_fill_manual(palette=mycolors, values=breaklabel(11), name="Average of Dry Spell", drop=FALSE, guide = guide_legend(reverse = T))+theme_bw()

last<-l+geom_polygon(data = Africa, aes(x = long,y = lat, group = group), fill = NA,color = "black",size = 1.1)+ theme(legend.position = c(.04, .04),legend.justification = c("left", "bottom"),legend.box.just = "right",legend.margin = margin(6, 6, 6, 6),legend.text = element_text(size=20),plot.title = element_text(hjust = 0.5,size=25,face = "bold"),axis.text.x = element_text(size=15,face = "bold"),axis.text.y = element_text(size=15,face = "bold"))

#last<-last+  annotation_custom(Im, xmin = MaxLon-5, xmax = MaxLon, ymin =MaxLat-5, ymax = MaxLat) +coord_cartesian(clip = "off")

last<-last+metR::scale_x_longitude(limits = c(MinLon,MaxLon),breaks =seq(MinLon,MaxLon,5))+scale_y_latitude(limits = c(MinLat,MaxLat),breaks =seq(MinLat,MaxLat,5))

last<-last+labs(title = Title,x="",y="")
#dir.create(paste("Products/Number_Rain_Day/Month2_5/Anomaly/",sep=""),recursive = T,showWarnings = F)

jpeg(filename = paste("Results/Monthly_dry/",Month_name,"/MeanDrySpell.jpeg",sep=""),
     width = 13,
     height =15,
     units = "in",
     res=300)
print(last)
dev.off()


################################################################################
################################################################################
################################################################################




# 
# dd=DATA_FMx
# colnames(dd)[3]='Max'
# colnames(DATA_10)[3]='Over10'
# dd=merge(dd,DATA_10,by=c("X","Y"))
# colnames(DATA_FMe)[c(3,4)]=c('Mean','Mean_T')
# dd=merge(dd,DATA_FMe,by=c("X","Y"))
# 
# 
# library(leaflet)
# pal <- colorFactor(c("green","#B2182B", "#D6604D", "#F4A582", "#FDDBC7" ), domain = c(0,1,2,3,4))
# 
# # "green","#B2182B", "#D6604D", "#F4A582", "#FDDBC7" ,"#F7F7F7" ,"#D1E5F0", "#92C5DE",
# # "#4393C3", "#2166AC", "#053061"
# 
# leaflet(dd) %>% addTiles() %>%
#   addCircleMarkers(~X, ~Y, radius = ~ifelse(Over10 >3,0.4, 0.4),
#                    color = ~pal(Over10),
#                    stroke = FALSE, fillOpacity = 0.5,
#                    popup = ~paste(
#                      
#                      paste('<b>', 'Nb of CDD Over 10 Days:', '</b>',round( Over10,0)),
#                      paste('<b>', 'Max of CDD:', '</b>', round(Max,0)), 
#                      paste('<b>', 'Average of CDD:', '</b>', round(Mean,2)), 
#                      sep = '<br/>')
#                    
#   )
