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

##### Water Balance Calculations 
wb_sites <- read.csv(paste0(OutDir, "WB_site_parameters.new.csv"))
n<-nrow(wb_sites)
WB_GCMs %>% filter(CF %in% CFs) -> WB_GCMs
T.Base = 0 

#Date format
DateFormat = "%m/%d/%Y"

#Output directory
#WBdir = file.path(FigDir,"water-balance/")# './figures/maps'
#if(dir.exists(WBdir) == FALSE){
#  dir.create(WBdir)
#}

#Select GCMs - Include RCP
unique(ALL_FUTURE$GCM)

############################################################ END USER INPUTS ###################################################################

############################################################ CREATE CLIMATE INPUTS #############################################################
### Historical
#Convert pr.In to mm and F to C
Gridmet$ppt_mm <- (Gridmet$PrcpIn*25.4)
Gridmet$tmax_C <- 5/9*(Gridmet$TmaxF - 32)
Gridmet$tmin_C <- 5/9*(Gridmet$TminF - 32)
Gridmet$tmean_C <- (Gridmet$tmax_C + Gridmet$tmin_C)/2


#### Projected
# Convert pr.In to mm
ALL_FUTURE$ppt_mm <- (ALL_FUTURE$PrcpIn*25.4)
ALL_FUTURE$tmax_C <- 5/9*(ALL_FUTURE$TmaxF - 32)
ALL_FUTURE$tmin_C <- 5/9*(ALL_FUTURE$TminF - 32)
ALL_FUTURE$tmean_C <- (ALL_FUTURE$tmax_C + ALL_FUTURE$tmin_C)/2
ALL_FUTURE <- ALL_FUTURE[-(which(ALL_FUTURE$Year <= max(Gridmet$Year))),]

#Add YrMon column

#if(dir.exists(WBdir) == FALSE){
#  dir.create(WBdir)
#}

# Subset selected GCMs
ClimData <- ALL_FUTURE %>% filter(GCM %in% WB_GCMs$GCM) %>% 
  select(c("Date","ppt_mm","tmean_C","GCM")) 
CD <- split(ClimData, ClimData$GCM)
for(i in 1:length(unique(ClimData$GCM))){
  gcm=unique(CD[[i]]$GCM)
  CD[[i]] <- rbind(CD[[i]], Gridmet %>% select(c("Date","ppt_mm","tmean_C","GCM")))
  CD[[i]]$GCM = gcm
}
ClimData<-do.call(rbind,CD)
rm(CD)
ClimData$GCM<-factor(ClimData$GCM,levels=unique(ClimData$GCM))
ClimData <- ClimData |> arrange(GCM, Date)
rownames(ClimData) <- NULL

######################################################### END CLIMATE INPUTS ####################################################################


######################################################### CALCULATE WB VARIABLES ################################################################
AllDailyWB<-list()
alldaily <- list()

for (j in 1:length(levels(ClimData$GCM))){
  gcm = levels(ClimData$GCM)[j]
  DailyWB = subset(ClimData,GCM==gcm)
  for(i in 1:nrow(wb_sites)){
    ID = wb_sites$WB_site[i]
    Lat = wb_sites$Lat[i]
    Lon = wb_sites$Lon[i]
    Elev = wb_sites$Elevation[i]
    Aspect = wb_sites$Aspect[i]
    Slope = wb_sites$Slope[i]
    SWC.Max = wb_sites$SWC.Max[i]
    Wind = wb_sites$Wind[i]
    Snowpack.Init = wb_sites$Snowpack.Init[i]
    Soil.Init = wb_sites$Soil.Init[i]
    Shade.Coeff = wb_sites$Shade.Coeff[i]
    
    #Calculate daily water balance variables 
    
    DailyWB$ID = ID
    DailyWB$doy <- yday(DailyWB$Date)
    DailyWB$daylength = get_daylength(DailyWB$Date, Lat)
    DailyWB$jtemp = as.numeric(get_jtemp(Lon, Lat))
    DailyWB$F = get_freeze(DailyWB$jtemp, DailyWB$tmean_C)
    DailyWB$RAIN = get_rain(DailyWB$ppt_mm, DailyWB$F)
    DailyWB$SNOW = get_snow(DailyWB$ppt_mm, DailyWB$F)
    DailyWB$MELT = get_melt(DailyWB$tmean_C, DailyWB$jtemp, hock=4, DailyWB$SNOW, Snowpack.Init)
    DailyWB$PACK = get_snowpack(DailyWB$jtemp, DailyWB$SNOW, DailyWB$MELT)
    DailyWB$W = DailyWB$MELT + DailyWB$RAIN
    if(PET_Method == "Hamon"){
      DailyWB$PET = ET_Hamon_daily(DailyWB)
    } else {
      if(PET_Method == "Penman-Monteith"){
        DailyWB$PET = ET_PenmanMonteith_daily(DailyWB)
      } else {
        if(PET_Method == "Oudin"){
          DailyWB$PET = get_OudinPET(DailyWB$doy, Lat, DailyWB$PACK, DailyWB$tmean_C, Slope, Aspect, Shade.Coeff)
        } else {
          print("Error - PET method not found")
        }
      }
    }
    DailyWB$PET = modify_PET(DailyWB$PET, Slope, Aspect, Lat, Shade.Coeff)
    DailyWB$W_PET = DailyWB$W - DailyWB$PET
    DailyWB$SOIL = get_soil(DailyWB$W, Soil.Init, DailyWB$PET, DailyWB$W_PET, SWC.Max)
    DailyWB$DSOIL = diff(c(Soil.Init, DailyWB$SOIL))
    DailyWB$AET = get_AET(DailyWB$W, DailyWB$PET, DailyWB$SOIL, Soil.Init)
    DailyWB$W_ET_DSOIL = DailyWB$W - DailyWB$AET - DailyWB$DSOIL
    DailyWB$D = DailyWB$PET - DailyWB$AET
    DailyWB$GDD = get_GDD(DailyWB$tmean_C, T.Base)
    alldaily[[i]] = DailyWB
  }
  
  AllDailyWB[[j]] = do.call(rbind,alldaily)
}

WBData<-do.call(rbind,AllDailyWB)
rm(ClimData)
######################################################### END WB VARIABLE CALCULATIONS ################################################################

######################################################### AGGREGATE OUTPUTS TO MONTLY/ANNUAL ################################################################
# WBData <- subset(WBData, GCM %in% WB_GCMs$GCM | GCM == "gridmet.historical")

WBData$yrmon = strftime(WBData$Date, "%Y%m")
WBData$year = strftime(WBData$Date, "%Y")

#Monthly
MonthlyWB = aggregate(ppt_mm~yrmon+GCM,data=aggregate(ppt_mm~yrmon+GCM+ID,data=WBData,sum),mean)
colnames(MonthlyWB)[3]<-"sum_p.mm"

MonthlyWB$avg_t.C = aggregate(tmean_C ~ yrmon+GCM, data=WBData, FUN=mean)[,3]
MonthlyWB$sum_rain.mm = aggregate(RAIN~yrmon+GCM,data=aggregate(RAIN~yrmon+GCM+ID,data=WBData,sum),mean)[,3]
MonthlyWB$sum_snow.mm = aggregate(SNOW~yrmon+GCM,data=aggregate(SNOW~yrmon+GCM+ID,data=WBData,sum),mean)[,3]
MonthlyWB$max_pack.mm = aggregate(PACK~yrmon+GCM,data=aggregate(PACK~yrmon+GCM+ID,data=WBData,max),mean)[,3]
MonthlyWB$sum_melt.mm = aggregate(MELT~yrmon+GCM,data=aggregate(MELT~yrmon+GCM+ID,data=WBData,sum),mean)[,3]
MonthlyWB$sum_w.mm = aggregate(W~yrmon+GCM,data=aggregate(W~yrmon+GCM+ID,data=WBData,sum),mean)[,3]
MonthlyWB$sum_pet.mm = aggregate(PET~yrmon+GCM,data=aggregate(PET~yrmon+GCM+ID,data=WBData,sum),mean)[,3]
MonthlyWB$sum_w_pet.mm = aggregate(W_PET~yrmon+GCM,data=aggregate(W_PET~yrmon+GCM+ID,data=WBData,sum),mean)[,3]
MonthlyWB$avg_soil.mm = aggregate(SOIL ~ yrmon+GCM, data=WBData, FUN=mean)[,3]
MonthlyWB$sum_aet.mm = aggregate(AET~yrmon+GCM,data=aggregate(AET~yrmon+GCM+ID,data=WBData,sum),mean)[,3]
MonthlyWB$runoff.mm = aggregate(W_ET_DSOIL~yrmon+GCM,data=aggregate(W_ET_DSOIL~yrmon+GCM+ID,data=WBData,sum),mean)[,3]
MonthlyWB$sum_d.mm = aggregate(D~yrmon+GCM,data=aggregate(D~yrmon+GCM+ID,data=WBData,sum),mean)[,3]
MonthlyWB$sum_gdd.mm = aggregate(GDD~yrmon+GCM,data=aggregate(GDD~yrmon+GCM+ID,data=WBData,sum),mean)[,3]

#Annual
AnnualWB = aggregate(ppt_mm ~ year+GCM, data=aggregate(ppt_mm~year+GCM+ID,data=WBData,sum), mean)
colnames(AnnualWB)[3]<-"sum_p.mm"

AnnualWB$avg_t.C = aggregate(tmean_C ~ year+GCM, data=WBData, FUN=mean)[,3]
AnnualWB$sum_rain.mm = aggregate(RAIN ~ year+GCM, data=aggregate(RAIN~year+GCM+ID,data=WBData,sum), mean)[,3]
AnnualWB$sum_snow.mm = aggregate(SNOW ~ year+GCM, data=aggregate(SNOW~year+GCM+ID,data=WBData,sum), mean)[,3]
AnnualWB$max_pack.mm = aggregate(PACK ~ year+GCM, data=aggregate(PACK~year+GCM+ID,data=WBData,max), mean)[,3]
AnnualWB$sum_melt.mm = aggregate(MELT ~ year+GCM, data=aggregate(MELT~year+GCM+ID,data=WBData,sum), mean)[,3]
AnnualWB$sum_w.mm = aggregate(W ~ year+GCM, data=aggregate(W~year+GCM+ID,data=WBData,sum), mean)[,3]
AnnualWB$sum_pet.mm = aggregate(PET ~ year+GCM, data=aggregate(PET~year+GCM+ID,data=WBData,sum), mean)[,3]
AnnualWB$sum_w_pet.mm = aggregate(W_PET ~ year+GCM, data=aggregate(W_PET~year+GCM+ID,data=WBData,sum), mean)[,3]
AnnualWB$avg_soil.mm = aggregate(SOIL ~ year+GCM, data=WBData, FUN=mean)[,3]
AnnualWB$sum_aet.mm = aggregate(AET ~ year+GCM, data=aggregate(AET~year+GCM+ID,data=WBData,sum), mean)[,3]
AnnualWB$runoff.mm = aggregate(W_ET_DSOIL ~ year+GCM, data=aggregate(W_ET_DSOIL~year+GCM+ID,data=WBData,sum), mean)[,3]
AnnualWB$sum_d.mm = aggregate(D ~ year+GCM, data=aggregate(D~year+GCM+ID,data=WBData,sum), mean)[,3]
AnnualWB$sum_gdd.C = aggregate(GDD ~ year+GCM, data=aggregate(GDD~year+GCM+ID,data=WBData,sum), mean)[,3]

MonthlyWB %>% mutate(across(3:length(MonthlyWB), round, 1)) %>% write.csv(.,paste0(TableDir,"WB-Monthly.csv"),row.names=FALSE)
AnnualWB %>% mutate(across(3:length(AnnualWB), round, 1)) %>% write.csv(paste0(TableDir,"WB-Annual-new.csv"),row.names=FALSE)

####### Timeseries plots ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

AnnualWB <- read.csv(paste0(TableDir,"WB-Annual-new.csv")) |> 
  mutate(max_pack.in = max_pack.mm/ 25.4,
         runoff.in = runoff.mm/ 25.4,
         sum_d.in = sum_d.mm/ 25.4,
         sum_aet.in = sum_aet.mm/ 25.4) %>% dplyr::rename(Year=year) |>
  mutate(GCM = ifelse(Year < 2023, "Historical", GCM)) |> 
  left_join(WB_GCMs,by="GCM") |> 
  mutate(CF = ifelse(is.na(CF), "Historical", CF),
         CF = factor(CF, levels=c("Historical",CFs))) 


WBAvgs <- aggregate(cbind(sum_d.in, runoff.in, max_pack.in)~Year+CF, AnnualWB, sum) %>% 
  group_by(CF) %>% 
  mutate(D.inRoll10 = rollmean(sum_d.in, rollLen, fill=NA, align="right"),
         Runoff.inRoll10 = rollmean(runoff.in, rollLen, fill=NA, align="right"),
         SWEaccum.inRoll10 = rollmean(max_pack.in, rollLen, fill=NA, align="right"),
         Year = as.numeric(Year))

col2 <- c("darkgray",rev(colors2)) 

d<-LT_plot(WBAvgs,sum_d.in,rollvar=D.inRoll10,cols=col,yaxis="Mean annual climatic \nwater deficit (in/year)",title="",CFmethod="I")
ggsave("D.in-Timeseries.png", d+labs(title=paste0(SiteID,"-Historical and future\n mean annual climatic water deficit (inches/Yr)")), path = FigDir, height=PlotHeight, width=PlotWidth)