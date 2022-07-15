####### Batch water balance code #######
# Update from Amanda batch code - to bring work from climate data 

############################################################# USER INPUTS ##################################################################### 

#Formatted input data as a daily time series. Needs to include the following columns: Date, ppt_mm, tmax_C, tmin_C, and tmean_C (temp.'s in deg. Celsius)

DataFile <- list.files(path = file.path(OutDir,"input-data/"), pattern = 'Final_Environment.RData', full.names = TRUE) # Environment needs to be added if not parsing MACA data
load(DataFile)


#rm(list=setdiff(ls(), c("ALL_HIST","ALL_FUTURE","site","CF_GCM")))

#Site characteristics 
#sites = read.csv("C:/Users/adillon/Documents/RSS/CONG/WB/CONG_site_characteristics.csv") #CSV file containing properties for all sites
n<-nrow(wb_sites)
#Threshold temperature (deg C) for growing degree-days calculation
T.Base = 0 

#Method for PET calculation 
Method = "Oudin"  #Hamon is default method for daily PRISM and MACA data (containing only Tmax, Tmin, and Date). 

#Date format
DateFormat = "%m/%d/%Y"

#Output directory
WBdir = file.path(FigDir,"water-balance/")# './figures/maps'
if(dir.exists(WBdir) == FALSE){
  dir.create(WBdir)
}

#Select GCMs - Include RCP
unique(ALL_FUTURE$GCM)

colors3<-c("gray",colors2)
############################################################ END USER INPUTS ###################################################################

############################################################ CREATE CLIMATE INPUTS #############################################################
#### Historical
# Convert pr.In to mm and F to C
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
#Add YrMon column


if(dir.exists(WBdir) == FALSE){
  dir.create(WBdir)
}

ClimData<-data.frame(Date=as.numeric(),ppt_mm=as.numeric(),tmean_C=as.numeric(),GCM=as.character())
# Loop through selected GCMs
ClimData <- ALL_FUTURE %>% filter(GCM %in% WB_GCMs$GCM) %>% 
  select(c("Date","ppt_mm","tmean_C","GCM")) %>%
  bind_rows(Gridmet %>% select(c("Date","ppt_mm","tmean_C","GCM")))

#for(i in 1:nrow(WB_GCMs)){
#  gcm <- WB_GCMs$GCM[i]
#  x<-subset(Gridmet, select=c("Date","ppt_mm","tmean_C","GCM"))
#  y<-subset(ALL_FUTURE,GCM == gcm, select=c("Date","ppt_mm","tmean_C","GCM"))
#  ClimData = rbind(ClimData,x,y)
#}
ClimData$GCM<-factor(ClimData$GCM,levels=unique(ClimData$GCM))

WB_GCMs <- WB_GCMs %>% add_row(GCM = Gridmet$GCM, C = 0)
######################################################### END CLIMATE INPUTS ####################################################################


######################################################### CALCULATE WB VARIABLES ################################################################
AllDailyWB<-list()

for (j in 1:length(levels(ClimData$GCM))){
  gcm = levels(ClimData$GCM)[j]
  DailyWB = subset(ClimData,GCM=gcm)
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
    if(Method == "Hamon"){
      DailyWB$PET = ET_Hamon_daily(DailyWB)
    } else {
      if(Method == "Penman-Monteith"){
        DailyWB$PET = ET_PenmanMonteith_daily(DailyWB)
      } else {
        if(Method == "Oudin"){
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
    AllDailyWB[[i]] = DailyWB
  }
}
WBData<-do.call(rbind,AllDailyWB)
######################################################### END WB VARIABLE CALCULATIONS ################################################################

######################################################### AGGREGATE OUTPUTS TO MONTLY/ANNUAL ################################################################

WBData$yrmon = strftime(WBData$Date, "%Y%m")
WBData$year = strftime(WBData$Date, "%Y")

#Monthly
MonthlyWB = aggregate(ppt_mm~yrmon+GCM,data=aggregate(ppt_mm~yrmon+GCM+ID,data=WBData,sum),mean)
colnames(MonthlyWB)[3]<-"sum_p"

MonthlyWB$avg_t = aggregate(tmean_C ~ yrmon+GCM, data=WBData, FUN=mean)[,3]
MonthlyWB$sum_rain = aggregate(RAIN~yrmon+GCM,data=aggregate(RAIN~yrmon+GCM+ID,data=WBData,sum),mean)[,3]
MonthlyWB$sum_snow = aggregate(SNOW~yrmon+GCM,data=aggregate(SNOW~yrmon+GCM+ID,data=WBData,sum),mean)[,3]
MonthlyWB$max_pack = aggregate(PACK ~ yrmon+GCM, data=WBData, FUN=max)[,3]
MonthlyWB$sum_melt = aggregate(MELT~yrmon+GCM,data=aggregate(MELT~yrmon+GCM+ID,data=WBData,sum),mean)[,3]
MonthlyWB$sum_w = aggregate(W~yrmon+GCM,data=aggregate(W~yrmon+GCM+ID,data=WBData,sum),mean)[,3]
MonthlyWB$sum_pet = aggregate(PET~yrmon+GCM,data=aggregate(PET~yrmon+GCM+ID,data=WBData,sum),mean)[,3]
MonthlyWB$sum_w_pet = aggregate(W_PET~yrmon+GCM,data=aggregate(W_PET~yrmon+GCM+ID,data=WBData,sum),mean)[,3]
MonthlyWB$avg_soil = aggregate(SOIL ~ yrmon+GCM, data=WBData, FUN=mean)[,3]
MonthlyWB$sum_aet = aggregate(AET~yrmon+GCM,data=aggregate(AET~yrmon+GCM+ID,data=WBData,sum),mean)[,3]
MonthlyWB$sum_w_et_dsoil = aggregate(W_ET_DSOIL~yrmon+GCM,data=aggregate(W_ET_DSOIL~yrmon+GCM+ID,data=WBData,sum),mean)[,3]
MonthlyWB$sum_d = aggregate(D~yrmon+GCM,data=aggregate(D~yrmon+GCM+ID,data=WBData,sum),mean)[,3]
MonthlyWB$sum_gdd = aggregate(GDD~yrmon+GCM,data=aggregate(GDD~yrmon+GCM+ID,data=WBData,sum),mean)[,3]

#Annual
AnnualWB = aggregate(ppt_mm ~ year+GCM, data=aggregate(ppt_mm~year+GCM+ID,data=WBData,sum), mean)
colnames(AnnualWB)[3]<-"sum_p"
AnnualWB$avg_t = aggregate(tmean_C ~ year+GCM, data=WBData, FUN=mean)[,3]
AnnualWB$sum_rain = aggregate(RAIN ~ year+GCM, data=aggregate(RAIN~year+GCM+ID,data=WBData,sum), mean)[,3]
AnnualWB$sum_snow = aggregate(SNOW ~ year+GCM, data=aggregate(SNOW~year+GCM+ID,data=WBData,sum), mean)[,3]
AnnualWB$max_pack = aggregate(PACK ~ year+GCM, data=aggregate(PACK~year+GCM+ID,data=WBData,max), mean)[,3]
AnnualWB$sum_melt = aggregate(MELT ~ year+GCM, data=aggregate(MELT~year+GCM+ID,data=WBData,sum), mean)[,3]
AnnualWB$sum_w = aggregate(W ~ year+GCM, data=aggregate(W~year+GCM+ID,data=WBData,sum), mean)[,3]
AnnualWB$sum_pet = aggregate(PET ~ year+GCM, data=aggregate(PET~year+GCM+ID,data=WBData,sum), mean)[,3]
AnnualWB$sum_w_pet = aggregate(W_PET ~ year+GCM, data=aggregate(W_PET~year+GCM+ID,data=WBData,sum), mean)[,3]
AnnualWB$avg_soil = aggregate(SOIL ~ year+GCM, data=WBData, FUN=mean)[,3]
AnnualWB$sum_aet = aggregate(AET ~ year+GCM, data=aggregate(AET~year+GCM+ID,data=WBData,sum), mean)[,3]
AnnualWB$sum_w_et_dsoil = aggregate(W_ET_DSOIL ~ year+GCM, data=aggregate(W_ET_DSOIL~year+GCM+ID,data=WBData,sum), mean)[,3]
AnnualWB$sum_d = aggregate(D ~ year+GCM, data=aggregate(D~year+GCM+ID,data=WBData,sum), mean)[,3]
AnnualWB$sum_gdd = aggregate(GDD ~ year+GCM, data=aggregate(GDD~year+GCM+ID,data=WBData,sum), mean)[,3]

write.csv(MonthlyWB, file.path(TableDir,"MonthlyWB.csv"), row.names=F)
write.csv(AnnualWB,file.path(TableDir,"AnnualWB.csv"), row.names=F)

