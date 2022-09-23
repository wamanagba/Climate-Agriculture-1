library(ncdf4)
library(dplyr)
library(rio)
library(rgdal)
library(ggplot2)
rm(list = ls())

Data_Source="CPC-UNIFIED"

Africa<-readOGR("/Climate-Agriculture-1/SHP_AFRIQUE/Afrique_frontier_news.shp") 
#Data_Source="CPC-UNIFIED"
MinLon=-25
MaxLon=55
MinLat=-40
MaxLat=40


setwd("/Climate-Agriculture-1/DrySpell/")
source("ScriptS/Dry_spell_Funct.R")

k=2000
cpt=7
A=c("Jan", "Feb", "Mar", "Apr", "May", "Jun" ,"Jul", "Aug", "Sep" ,"Oct", "Nov", "Dec")
for(cpt in 1:12){
  cpt1=(cpt+1)%%12
  if(cpt1==0) cpt1=12
  cpt2=(cpt+2)%%12
  if(cpt2==0) cpt2=12
  season=paste(substr(A[cpt],1,1),substr(A[cpt1],1,1),substr(A[cpt2],1,1),sep = "")
  
for (k in 1981:2010) {
  print(k);print(season)
  if(k==1981){
    Data<-rio::import(paste("Data/CPC-UNIFIED/CSV_Format/",k,".csv",sep=""))
    Data$Year<-format(Data$T,"%Y")
    Data$Month=(format(Data$T,"%b"))
    Data$rain[is.na(Data$rain)]=0
    Data= filter(Data,Month %in% A[c(cpt,cpt1,cpt2)])

    
    Sum=Data%>%
      group_by(X,Y,Year)%>%
      summarise(clim=sum(rain))
    
    Data_Dry_Speel<-Data%>%
      group_by(X,Y,Year)%>%
      summarise(Spell=CDD_function(rain,2.5,Nb=10))
    
    Data_F<-merge(Sum,Data_Dry_Speel,by=c("X","Y","Year"))   
    
  
  }else{
    Data1<-rio::import(paste("Data/CPC-UNIFIED/CSV_Format/",k,".csv",sep=""))
    Data1$Year<-format(Data1$T,"%Y")

    Data1$Month=(format(Data1$T,"%b"))
    Data1$rain[is.na(Data1$rain)]=0
    Data1= filter(Data1,Month %in% A[c(cpt,cpt1,cpt2)])

    
    Sum=Data1%>%
      group_by(X,Y,Year)%>%
      summarise(clim=sum(rain))
    
    Data_Dry_Speel1<-Data1%>%
      group_by(X,Y,Year)%>%
      summarise(Spell=CDD_function(rain,2.5,Nb=10))
    
    Data_Dry_Speel1<-merge(Sum,Data_Dry_Speel1,by=c("X","Y","Year"))  
   
    Data_F<-rbind(Data_F,Data_Dry_Speel1)
  }
}


Data_Dry<-Data_F%>%
  group_by(X,Y)%>%
  summarise(Mean=mean(Spell))

Clima<-Data_F%>%
  group_by(X,Y)%>%
  summarise(climatology=mean(clim))

Clima$binary= ifelse(Clima$climatology>100,1,0)

Data_final=merge(Clima,Data_Dry,by=c("X","Y"))


dir.create(paste("Data/Dry_Speel_Data/Climatologie/",sep = ""),recursive = T, showWarnings = F)
rio::export(Data_final,paste("Data/Dry_Speel_Data/Climatologie/1981_2010_",season,".csv",sep = ""))






 # Create a Maps


Data=filter(Data_final,binary==1)

Data_F=Data

Raster_file<-rasterFromXYZ(Data_F[c("X","Y","Mean")])

Raster_file_1=disaggregate(Raster_file,8,method='bilinear')

rr = raster::mask(Raster_file_1 ,Africa)

Data_F <- as.data.frame(rasterToPoints(rr ))
#rio::export(Data,"Data/Annual_Total_Mean_1983_2021_CHIRPS.csv")
mybreaks <- c(0,1,2,3,4,5,Inf)

#Function to return the desired number of colors

mycolors<- function(x) {
  colors<-colorRampPalette(c("#8cb02c","blue","darkviolet","#37fdf8","red","#89522a"))(6)
  colors[1:x]
}

#Function to create labels for legend

breaklabel <- function(x){
  labels<- as.character(c(0,1,2,3,4,5))
  labels[1:x]
}
################################################################################

#Title<-paste("Number of dry spell overs 10 days", "\nRef: 1981-2010","\nData Source: ",Data_Source,"\n Season:",season,sep="")
Title<-toupper(paste("Number of dry spell overs 10 days","\nRef: 1981-2010","\nData Source: ",Data_Source,sep=""))

#Im<-grid::rasterGrob(png::readPNG("Logos/Acmad_logo_1.png"), interpolate = TRUE)

l<-ggplot()+geom_contour_filled(data=Data_F, aes(x,y,z =Mean),breaks= mybreaks, show.legend = TRUE) +
  scale_fill_manual(palette=mycolors, values=breaklabel(6), name="", drop=FALSE, guide = guide_legend(reverse = T))+theme_bw()

last<-l+geom_polygon(data = Africa, aes(x = long,y = lat, group = group), fill = NA,color = "black",size = 1.1)+ theme(legend.position = c(.04, .04),legend.justification = c("left", "bottom"),legend.box.just = "right",legend.margin = margin(6, 6, 6, 6),legend.text = element_text(size=20),plot.title = element_text(hjust = 0.5,size=25,face = "bold"),axis.text.x = element_text(size=15,face = "bold"),axis.text.y = element_text(size=15,face = "bold"))

#last<-last+  annotation_custom(Im, xmin = MaxLon-5, xmax = MaxLon, ymin =MaxLat-5, ymax = MaxLat) +coord_cartesian(clip = "off")

last<-last+metR::scale_x_longitude(limits = c(MinLon,MaxLon),breaks =seq(MinLon,MaxLon,5))+scale_y_latitude(limits = c(MinLat,MaxLat),breaks =seq(MinLat,MaxLat,5))

last<-last+labs(title = Title,x="",y="")
dir.create(paste("Products/Dry_speel/Afrique/Climatology/",sep=""),recursive = T,showWarnings = F)

jpeg(filename = paste("Products/Dry_speel/Afrique/Climatology/",season,".jpeg",sep=""),
     width = 14,
     height =14,
     units = "in",
     res=300)
print(last)
dev.off()



}
