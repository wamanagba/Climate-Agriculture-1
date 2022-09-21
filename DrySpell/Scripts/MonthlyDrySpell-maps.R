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

season='JAS'
Mask=rio::import(paste("E:/ACMAD_Git/Data/CPC-UNIFIED/Mask/mask_ ",season," .csv",sep = ""))
#Mask File
# 
# dir.create("Mask")
# download.file(paste("http://154.66.220.45:8080/thredds/fileServer/ACMAD/CDD/climatedataservice/Seasonal_Climate_Zones/Masked/CAMS_OPI/1981-2010/Climatology_",Start_Month,"_",End_Month,"_1981_2010_CAMS_Masked.nc",sep=""),mode = "wb",paste("Mask/",Season_Name,".nc",sep=""))

# Mask<-tidync::tidync(paste("Mask/",Season_Name,".nc",sep=""))%>%hyper_tibble()
# names(Mask)[2]="X"
# names(Mask)[3]="Y"

DATA_last=merge(Data_Dry_Speel_last,Mask,by=c("X","Y"))
# DATA_last$Spell=ifelse(DATA_last$mask==-1,-5,DATA_last$Spell)
DATA_L=filter(DATA_last,mask==1)
# dir.create("Data/Monthly_dry/",recursive = T,showWarnings = F)
# rio::export(DATA_last,paste("Data/Monthly_dry/",Month_name,"Spellast.csv",sep=""))  


DATA_F10=merge(Data_Dry_Speel10,Mask,by=c("X","Y"))
# DATA_F10$Spell=ifelse(DATA_F10$mask==-1,-5,DATA_F10$Spell)
DATA_10=filter(DATA_F10,mask==1)
# dir.create("Data/Monthly_dry/",recursive = T,showWarnings = F)
# rio::export(DATA_F10,paste("Data/Monthly_dry/",Month_name,"Spell10.csv",sep=""))  

DATA_FMax=merge(Data_Dry_SpeelMax,Mask,by=c("X","Y"))
# DATA_FMax$Spell=ifelse(DATA_FMax$mask==-1,-5,DATA_FMax$Spell)
DATA_FMx=filter(DATA_FMax,mask==1)
# rio::export(DATA_FMax,paste("Data/Monthly_dry/",Month_name,"SpellMax2.csv",sep=""))  
# 

DATA_FMean=merge(Data_Dry_SpeelMean,Mask,by=c("X","Y"))
# DATA_FMean$Spell=ifelse(DATA_FMean$mask==-1,-5,DATA_FMean$Spell)
DATA_FMe=filter(DATA_FMean,mask==1)
# rio::export(DATA_FMean,paste("Data/Monthly_dry/",Month_name,"SpellMean.csv",sep=""))  


#############################################
###############################################
###############################################

# d_DATA_F10=DATA_10
# Raster_file<-rasterFromXYZ(DATA_10[c("X","Y","Spell")])
# 
# Raster_file_1=disaggregate(Raster_file,20,method='bilinear')
# 
# rr = raster::mask(Raster_file_1 ,Africa)
# 
# DATA_10 <- as.data.frame(rasterToPoints(rr ))
# 
# ################################################################################
# ################################################################################
# ################################################################################
# 
# 
# 
# 
# d_DATA_last=DATA_L
# Raster_file<-rasterFromXYZ(DATA_L[c("X","Y","Spell")])
# 
# Raster_file_1=disaggregate(Raster_file,20,method='bilinear')
# 
# rr = raster::mask(Raster_file_1 ,Africa)
# 
# DATA_L <- as.data.frame(rasterToPoints(rr ))
# 
# 
# 
# 
# 
# #############################################
# ###############################################
# ###############################################
# 
# d_DATA_FMax=DATA_FMx
# Raster_file<-rasterFromXYZ(DATA_FMx[c("X","Y","Spell")])
# 
# Raster_file_1=disaggregate(Raster_file,20,method='bilinear')
# 
# rr = raster::mask(Raster_file_1 ,Africa)
# 
# DATA_FMx <- as.data.frame(rasterToPoints(rr ))
# 
# 
# 
# #############################################
# ###############################################
# ###############################################
# 
# d_DATA_FMean=DATA_FMe
# Raster_file<-rasterFromXYZ(DATA_FMe[c("X","Y","Spell")])
# 
# Raster_file_1=disaggregate(Raster_file,20,method='bilinear')
# 
# rr = raster::mask(Raster_file_1 ,Africa)
# 
# DATA_FMe <- as.data.frame(rasterToPoints(rr ))



################################################################################
################################################################################
################################################################################





dd=DATA_FMx
colnames(dd)[3]='Max'
colnames(DATA_10)[3]='Over10'
dd=merge(dd,DATA_10,by=c("X","Y"))
colnames(DATA_FMe)[c(3,4)]=c('Mean','Mean_T')
dd=merge(dd,DATA_FMe,by=c("X","Y"))


library(leaflet)
pal <- colorFactor(c("green","#B2182B", "#D6604D", "#F4A582", "#FDDBC7" ), domain = c(0,1,2,3,4))

# "green","#B2182B", "#D6604D", "#F4A582", "#FDDBC7" ,"#F7F7F7" ,"#D1E5F0", "#92C5DE",
# "#4393C3", "#2166AC", "#053061"

leaflet(dd) %>% addTiles() %>%
  addCircleMarkers(~X, ~Y, radius = ~ifelse(Over10 >3,0.4, 0.4),
                   color = ~pal(Over10),
                   stroke = FALSE, fillOpacity = 0.5,
                   popup = ~paste(
                     
                     paste('<b>', 'Nb of CDD Over 10 Days:', '</b>',round( Over10,0)),
                     paste('<b>', 'Max of CDD:', '</b>', round(Max,0)), 
                     paste('<b>', 'Average of CDD:', '</b>', round(Mean,2)), 
                     sep = '<br/>')
                   
  )
