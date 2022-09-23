
#Soil Moisture given month
#http://iridl.ldeo.columbia.edu/expert/expert/SOURCES/.NOAA/.NCEP/.CPC/.GMSM/.w/Y/-40/0.5/40/GRID/X/25/0.5/55/GRID/T/(%20Jun%202020)/VALUES/data.nc

#Climatology soil Moisture 1991-2020
#http://iridl.ldeo.columbia.edu/expert/expert/SOURCES/.NOAA/.NCEP/.CPC/.GMSM/.w/T/(Jun%201991)/(Jun%202020)/RANGEEDGES/Y/-40/0.5/40/GRID/X/25/0.5/55/GRID/T/12/STEP/%5BT%5Daverage/data.nc

#SPI Index
#http://iridl.ldeo.columbia.edu/expert/expert/SOURCES/a%3A/.IRI/.Analyses/.SPI/.SPI-CAMSOPI_1-Month/T/(Jun%202020)/VALUES/X/-25/0.5/55/GRID/Y/-40/0.5/40/GRID/data.nc

#Percentage of Average

#Monthly Cumulative
#http://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP/.CPC/.CAMS_OPI/.v0208/.mean/.prcp/Y/-40/0.5/40/GRID/X/25/0.5/55/GRID/T/(%20Jun%202020)/VALUES/31/mul/data.nc

#Climatology
#http://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP/.CPC/.CAMS_OPI/.v0208/.mean/.prcp/T/(Jun%201991)/(Jun%202020)/RANGEEDGES/Y/-40/0.5/40/GRID/X/25/0.5/55/GRID/T/12/STEP/%5BT%5Daverage/31/mul/data.nc
################################################################################
# Script for the Drought Monitoring: ACMAD
#
# Input Data: Monthly Data: Monthly Cumulative, Africa Shape file
#
# Product Output: 4 Maps for the Drought Monitoring Bulletins
# 
# Copyright: ACMAD/Niamey-Niger
# 
# Realized at DCE
# 
# For any assistance please contact: ibrahim.d.dije@aims-senegal.org
#                                  : sosnku2002@yahoo.com
#
######################################################################################################################################################## 
#Make sure you installed all the necessary package
rm(list=ls())
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
#options(warn = -1)
setwd("/Climate-Agriculture-1/Drought_Monitoring_Package/")
######################################################################
path="/Climate-Agriculture-1/Drought_Monitoring_Package/"

Month="Jan"
Month_name="January"
Year=2021
######################################################################
# Parameters<-data.frame(Month_Abbr=Month,Month_name=Month_name,Year=Year,path=path)

# rio::export(Parameters,"Parameter/Parameters.csv")
  
source("Scripts/Soil_Moisture_Data_Map.R")

source("Scripts/Percentage_Data_Map.R")

source("Scripts/SPI_Data_Map.R")

source("Scripts/Drought_Index_Data_Map.R")