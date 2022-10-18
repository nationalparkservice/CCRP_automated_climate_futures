#This script runs through 'initials' needed to run drought script from RCF output. 
#Lines requiring user input denoted by double #### blocks - lines 16 and 20
#After these inputs are entered, The rest of the initials (through line XX) can be run


library(here); library(plyr); # Use here::here when package lubridate is used
library(plotrix); library(zoo); library(ggplot2); library(grid); library(cowplot); library(reshape2); library(raster); library(ncdf4); library(reshape2); library(WriteXLS); library(data.table); library(RColorBrewer); library(ggrepel); library(lubridate); library(dplyr); library(forcats); library(openxlsx); library("WaterBalance"); library(sf); library(raster); library(rgdal); library(R.utils); library(tmap); library(tmaptools); library(ggmap); library(ggspatial);
library(gridExtra); library(SPEI); library(tidyr); library(tibble); library(sp); library(skimr); library(cft); library(stringr); library(ggpubr); library(lemon); library(ggfortify); library(extRemes)

rm(list=ls())
##################
##################
OutDir <- 'C:/Users/achildress/Documents/RCF_Testing/BAND/'#location where all of your output is saved
##################
##################

load(paste0(OutDir,"input-data/Final_Environment.RData")) #This will get data loaded through chunk Climate-Futures

##################
##################
CF_selected_updates <- "WarmWet_HotDry" #Select your CF by commenting out set you do not wish to use
# CF_selected_updates <- "WarmDry_HotWet" #Select your CF by commenting out set you do not wish to use
##################
##################

if(CF_selected_updates == "WarmWet_HotDry") {
FutureSubset <- CFs_all[c(1,5)]; CFs = FutureSubset  # Pick pair of climate futures.
CF_abbreviation <- "WW-HD"
colors2<- colors5[c(1,4)] # Select pair of climate futures - WarmWet/HotDry
colors3<-c("white",colors2)
col<- c("darkgray",colors2)  # WarmWet/HotDry
CFDir = paste0(OutDir,"WarmWet_HotDry/") # for .csv's
} else{
  FutureSubset <- CFs_all[c(4,2)]; CFs = FutureSubset  # Pick pair of climate futures.
  CF_abbreviation <- "WD-HW"
  colors2<- colors5[c(3,2)] # Select pair of climate futures - HotWet/WarmDry
  colors3<-c("white",colors2)
  # col<- c("darkgray","#9A9EE5","#E10720")  # WarmWet/HotDry
  col<- c("darkgray", colors2)  # HotWet/WarmDry
  CFDir = paste0(OutDir,"WarmDry_HotWet/") # for .csv's
}
TableDir = paste0(CFDir,"tables/") # for .csv's
FigDir = paste0(CFDir,"figures/") # for .csv's


### From here can run:
# Threshold plots
# Water Balance
# Drought
# Return
