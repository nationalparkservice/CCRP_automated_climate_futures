# Run this script to get data read in for troubleshooting scripts

library(here); library(plyr); # Use here::here when package lubridate is used
library(plotrix); library(zoo); library(ggplot2); library(grid); library(cowplot); library(reshape2); library(raster); library(ncdf4); library(reshape2); library(WriteXLS); library(data.table); library(RColorBrewer); library(ggrepel); library(lubridate); library(dplyr); library(forcats); library(openxlsx); library("WaterBalance"); library(sf); library(raster); library(rgdal); library(R.utils); library(tmap); library(tmaptools); library(ggspatial); library(basemaps)
library(gridExtra); library(SPEI); library(tidyr); library(tibble); library(sp); library(skimr);  library(stringr); library(ggpubr); library(lemon); library(ggfortify); library(extRemes); library(sp);library(terra);library(rasterVis); library(sf)

rm(list=ls())
SiteID = "SWQ"
state = "NC"
OutDir <- "C:/Users/arunyon/3D Objects/Local-files/RCF_Testing/SWQ/SWQ/"
DataDir = paste0(OutDir,'input-data/')

DataFile <- list.files(path = DataDir, pattern = 'Final_Environment.RData', full.names = TRUE) # Environment needs to be added if not parsing MACA data
load(DataFile)

FutureSubset <- CFs_all[c(1,5)]; CFs = FutureSubset  # Pick pair of climate futures.
CF_abbreviation <- "WW-HD"
# WB_GCMs <- subset(WB_GCMs, CF %in% CFs)

colors2<- colors5[c(1,4)] # Select pair of climate futures - WarmWet/HotDry
#colors2<- c("#F3D3CB","#12045C")  # Select pair of climate futures - HotWet/WarmDry

colors3<-c("white",colors2)
col<- c("darkgray",colors2)  # WarmWet/HotDry
#col<- c("darkgray","#F3D3CB","#12045C")  # HotWet/WarmDry

OutDir <- "C:/Users/arunyon/3D Objects/Local-files/RCF_Testing/SWQ/SWQ/"
DataDir = paste0(OutDir,'input-data/')
CFDir = paste0(OutDir,"WarmWet_HotDry/") # for .csv's
TableDir = paste0(CFDir,"tables/") # for .csv's
FigDir = paste0(CFDir,"figures/") # for .csv's

source(here::here("scripts", "Threshold_Bar_Charts.R")) # Requires "PARK_lat_long_Final_Environment.RData". Outputs plots and Excel Workbook 

source(here::here("scripts","Daily_WB_Model.R"))

source(here::here("scripts", "Drought_characteristics.R"))

# source(here::here("scripts", "WBgrid_plotting.R")) # Run the Water Balance Model

source(here::here("scripts", "Return_Events.R"))

source(here::here("scripts", "Summary plots.R"))

source(here::here("scripts", "Report_plots.R"))