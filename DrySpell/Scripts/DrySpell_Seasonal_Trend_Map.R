################################################################################
#                                
#https://iridl.ldeo.columbia.edu/expert/expert/SOURCES/.NOAA/.NCEP/.CPC/.CAMS_OPI/.v0208/.mean/.prcp/T/(Aug%201981)/(Oct%202010)/RANGEEDGES/Y/-40/0.5/40/GRID/X/-25/0.5/55/GRID/T/3/runningAverage/T/12/STEP/92/mul/data.nc
################################################################################
library(rio)
library(dplyr)
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
require("fields")
library(zoo)
library(tidync)
library(Hmisc)
library(ncdf4)
rm(list = ls())

options(download.file.method="libcurl", url.method="libcurl")

options(download.file.extra = '--no-check-certificate')

setwd("E:/Climate-Agriculture-1/DrySpell/")
Africa<-readOGR("/ACMAD_Git/SHP_AFRIQUE/Afrique_frontier_news.shp")

###############################Define Your Parameters###########################
Start_Year=1980

End_Year=2021

# Start_Month<-"Feb"
# 
# End_Month<- "Apr"

Season_Name<-"FMA"

# Period<-paste(Start_Month,"-",End_Month,sep="")
# Index=0
# if(Season_Name %in% c("NDJ","DJF","ONDJ","NDJF","DJFM")){
#   
#   End_Year=End_Year+1
#   Index=1
#   
# }
##########################Domain################################################

MinLon= -25

MaxLon=55

MinLat=-40

MaxLat=40

################################################################################

#If the season includes 2 Years: Example NDJ, DJF set Index=1 else let it 0


Data_Source="CPC-UNIFIED"

################################################################################
Year<-Start_Year
# 
# Nbday_End<-Hmisc::monthDays(as.Date(paste(Year,"-",End_Month,"-01",sep=""),"%Y-%b-%d"))
# 
# Number_of_Days<-length(seq(as.Date(paste(End_Year-Index,"-",Start_Month,"-","01",sep=""),format = "%Y-%b-%d"),as.Date(paste(End_Year,"-",End_Month,"-",Nbday_End,sep=""),format = "%Y-%b-%d"),by="day"))
# 
# Number_of_Months<-length(seq(as.Date(paste(End_Year-Index,"-",Start_Month,"-","01",sep=""),format = "%Y-%b-%d"),as.Date(paste(End_Year,"-",End_Month,"-",Nbday_End,sep=""),format = "%Y-%b-%d"),by="month"))


#Set index to specify if it is season with 2 years



#################################################
dir.create("Data",recursive = T,showWarnings = F)
# download.file(paste("https://iridl.ldeo.columbia.edu/expert/expert/SOURCES/.NOAA/.NCEP/.CPC/.CAMS_OPI/.v0208/.mean/.prcp/T/(",Start_Month,"%20",Start_Year,")/(",End_Month,"%20",End_Year,")/RANGEEDGES/Y/-40/0.5/40/GRID/X/-25/0.5/55/GRID/T/",Number_of_Months,"/runningAverage/T/12/STEP/",Number_of_Days,"/mul/data.nc",sep=""),mode="wb",paste("Data/",Season_Name,"_CAMS.nc",sep=""))
# 
# Data<-nc_open(filename = paste("Data/",Season_Name,"_CAMS.nc",sep=""))
# Lon<-ncvar_get(Data,"X")
# Lat<-ncvar_get(Data,"Y")
# Val<-ncvar_get(Data,"prcp")
# nc_close(Data)
# t=0
# j=1981
# Max<-End_Year-Index
# for (j in Start_Year:Max) {
# 
#    Data<-data.frame(Lon=Lon[1],Lat=Lat,Year=j,Cum=Val[1,,t+1])
#   
#   for(i in 2:length(Lon)){
#     Data1<-data.frame(Lon=Lon[i],Lat=Lat,Year=j,Cum=Val[i,,t+1])
#     Data<-rbind(Data,Data1)
#   }
#   if(j==Start_Year){
#     ALL_Data<-Data
#   }
#   else{
#     ALL_Data<-rbind(ALL_Data,Data)
#   }
#   t<-t+1
#   
# }
k=1981
jj=1
Season_Name="JAS"
for (Season_Name in c("JFM","FMA","MAM","AMJ","MJJ","JJA","JAS","ASO","SON","OND","NDJ","DJF")) {
  
Data=data_frame()
for (k in 1981:2021) {
data=rio::import(paste("/ACMAD_Git/Data/CPC-UNIFIED/DataBase/Number_DrySpell10/",Season_Name,"/",k,".csv",sep = ""))
data$Year=k
Data=rbind(Data,data)
}
# rio::export(ALL_Data,"Products/Data/Cumulative_81_2021_JJA.csv")
# ALL_Data$Cum<-ALL_Data$Cum/Number_of_Months
# 
Trend<-Data%>%
  group_by(X,Y)%>%
  summarise(TrendValue=summary(lm(coredata(spell10)~index(Year)))$coefficients[2])

Trend$TrendValue<-Trend$TrendValue*10
# 
Pvalue<-Data%>%
  group_by(X,Y)%>%
  summarise(pvalue=summary(lm(coredata(spell10)~index(Year)))$coefficients[8])

######Mapping
#Shape file


# #Mask File
# dir.create("Mask")
# download.file(paste("http://154.66.220.45:8080/thredds/fileServer/ACMAD/CDD/climatedataservice/Seasonal_Climate_Zones/Masked/CAMS_OPI/1981-2010/Climatology_",Start_Month,"_",End_Month,"_1981_2010_CAMS_Masked.nc",sep=""),mode = "wb",paste("Mask/",Season_Name,".nc",sep=""))
# 
# Mask<-tidync::tidync(paste("Mask/",Season_Name,".nc",sep=""))%>%hyper_tibble()

#                      
# names(Mask)[2]="X"
# 
# names(Mask)[3]="Y"
# 
# Trend<-merge(Data,Mask,by=c("X","X"),all=T)
# 
# Trend$TrendValue<-ifelse(Trend$mask==-1,NA,Trend$TrendValue)
# 
# Pvalue<-merge(Pvalue,Mask,by=c("Lon","Lat"),all=T)
# 
# Pvalue$pvalue<-ifelse(Pvalue$mask==-1,NA,Pvalue$pvalue)

################################################################################

r<-rasterFromXYZ(Trend[,c("X","Y","TrendValue")])

Data_Interpolated<-disaggregate(r,10,method="bilinear")

Data_Interpolated<-raster::mask(Data_Interpolated,Africa)
  
writeRaster(Data_Interpolated,filename = paste("/ACMAD_Git/DrySpellTrend/TrendV_1981_2021_CAMS_",Season_Name,".nc",sep=""),varname="Trend",format="CDF",overwrite=TRUE)

################################################################################

##############################Mapping###########################################

Trend_Raster<-rasterFromXYZ(Trend)

Trend_Raster_Interpoleted<-disaggregate(Trend_Raster,10,"bilinear")

Trend_Raster_Interpoleted_Masked<-raster::mask(Trend_Raster_Interpoleted,Africa)

Trend_df<-as.data.frame(rasterToPoints(Trend_Raster_Interpoleted_Masked))

################################################################################

Pvalue_Raster<-rasterFromXYZ(Pvalue)

Pvalue_Raster_Interpoleted<-aggregate(Pvalue_Raster,fact=2)

Pvalue_Raster_Interpoleted_Masked<-raster::mask(Pvalue_Raster_Interpoleted,Africa)

Pvalue_df<-as.data.frame(rasterToPoints(Pvalue_Raster_Interpoleted_Masked))

################################################################################

mybreaks <- c(-Inf,-1,-0.9,-0.3,0,0.3,0.9,1,2,Inf)

#Function to return the dersired number of colors
mycolors<- function(x) {
  colors<-colorRampPalette(c("#1806ff","#4c97ff","green","#e4ebdd","#e4ebdd","#FFAF42","#FF8303","red"))(9)
  colors[1:x]
}

#Function to create labels for legend
breaklabel <- function(x){
  labels<- as.character(c("<",-1,-0.9,-0.3,0,0.3,0.9,1,2,">"))
  labels[1:x]
}



Title = paste("Dry Spell Over 10 Days Trend for ",Season_Name,"\nData Source:",Data_Source,sep="")

Im<-grid::rasterGrob(png::readPNG(paste("/ACMAD_Git/Logos/Acmad_logo_1.png",sep="")), interpolate = TRUE)

l<-ggplot(data = Pvalue_df,aes(x,y))+geom_contour_filled(data=Trend_df, aes(x,y,z =TrendValue),breaks= mybreaks, show.legend = TRUE) +
  scale_fill_manual(palette=mycolors, values=breaklabel(9), name="day/decade", drop=FALSE, guide = guide_legend(reverse = TRUE))+theme_bw()

last<-l+geom_polygon(data = Africa, aes(x = long,y = lat, group = group), fill = NA,color = "black",size = 1.1)+ theme(legend.position = c(.05, .05),legend.justification = c("left", "bottom"),legend.box.just = "right",legend.margin = margin(6, 6, 6, 6),legend.text = element_text(size=20,face = "bold"),legend.title =  element_text(size=15,face = "bold"),plot.title = element_text(hjust = 0.5,size=25,face = "bold"),axis.text.x = element_text(size=15,face = "bold"),axis.text.y = element_text(size=15,face = "bold"))

last<-last+  annotation_custom(Im, xmin = MaxLon-5, xmax = MaxLon, ymin =MaxLat-5, ymax = MaxLat) +coord_cartesian(clip = "off")

last<-last+labs(title = Title,x="",y="")+stat_subset(aes(subset =pvalue <=0.05),goem="point", size = 2.1,color="black")

last<-last+ metR::scale_x_longitude(limits = c(MinLon, MaxLon),breaks = seq(MinLon, MaxLon,10)) + metR:: scale_y_latitude(limits = c(MinLat, MaxLat),breaks = seq(MinLat, MaxLat,10)) 


dir.create(paste("/Climate-Agriculture-1/DrySpell/Results/Trend_maps/",Data_Source,"/",sep=""),recursive = T,showWarnings = F)

jpeg(filename = paste("/Climate-Agriculture-1/DrySpell/Results/Trend_maps/",Data_Source,"/",jj,"-",Season_Name,"_",Start_Year,"_",End_Year,".jpeg",sep=""),
     width = 10,
     height = 13,
     units = "in",
     res=300)
print(last)
dev.off()
jj=jj+1
}
