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
library(tidync)
MinLon=-25
MaxLon=55
MinLat=-40
MaxLat=40

rm(list = ls())
Africa<-readOGR("~/Desktop/ACMAD_Git/SHP_AFRIQUE/Afrique_frontier_news.shp") 
setwd("~/Desktop/ACMAD_Git/Malaria/")
# Data_source="NCEP-NCAR"
# dir.create(paste("Data/",Data_source,"/CSV",sep = ""),recursive = T,showWarnings = F)


Data<-tidync(paste("Data/GEFS/Weekly_Mean_2_20220813.nc",sep=""))%>%hyper_tibble(na.rm = F)
Data$t=round(Data$t-273.15,2)

# rio::export(Data,paste("Data/",Data_source,"/CSV/",Name,".csv",sep=""))