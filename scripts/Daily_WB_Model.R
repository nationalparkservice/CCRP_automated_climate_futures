##### Water Balance Calculations 


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
#WBdir = file.path(FigDir,"water-balance/")# './figures/maps'
#if(dir.exists(WBdir) == FALSE){
#  dir.create(WBdir)
#}

#Select GCMs - Include RCP
unique(ALL_FUTURE$GCM)

colors3<-c("gray",colors2)
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
#Add YrMon column


#if(dir.exists(WBdir) == FALSE){
#  dir.create(WBdir)
#}

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

WB_GCMs <- WB_GCMs %>% 
  add_row(GCM = unique(Gridmet$GCM), CF = "Historical")
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
WBData <- subset(WBData, GCM %in% WB_GCMs$GCM | GCM == "gridmet.historical")

WBData$yrmon = strftime(WBData$Date, "%Y%m")
WBData$year = strftime(WBData$Date, "%Y")

WBData <- subset(WBData, year >= Yr-Range/2 & year <= Yr+Range/2 | year <= 2012)

#Monthly
MonthlyWB = aggregate(ppt_mm~yrmon+GCM,data=aggregate(ppt_mm~yrmon+GCM+ID,data=WBData,sum),mean)
colnames(MonthlyWB)[3]<-"sum_p.mm"

MonthlyWB$avg_t.C = aggregate(tmean_C ~ yrmon+GCM, data=WBData, FUN=mean)[,3]
MonthlyWB$sum_rain.mm = aggregate(RAIN~yrmon+GCM,data=aggregate(RAIN~yrmon+GCM+ID,data=WBData,sum),mean)[,3]
MonthlyWB$sum_snow.mm = aggregate(SNOW~yrmon+GCM,data=aggregate(SNOW~yrmon+GCM+ID,data=WBData,sum),mean)[,3]
MonthlyWB$max_pack.mm = aggregate(PACK ~ yrmon+GCM, data=WBData, FUN=max)[,3]
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

write.csv(MonthlyWB, file.path(TableDir,"MonthlyWB.csv"), row.names=F)
write.csv(AnnualWB,file.path(TableDir,"AnnualWB.csv"), row.names=F)

################ WB GRID PLOTTING AFTER THIS............ ##################



######################################################### AGGREGATE OUTPUTS TO MONThLY/ANNUAL ################################################################



#MonthlyWB$max_SWEaccum.in = aggregate(SWEaccum.in~yrmon+GCM,data=WBData,max)[,3] #Max_pack -- conversion?
#MonthlyWB$sum_runoff.in = aggregate(Runoff.in~yrmon+GCM,data=aggregate(Runoff.in~yrmon+GCM,data=WBData,sum),mean)[,3] #AnnualWB$runoff -- conversion?
#MonthlyWB$sum_pet.in = aggregate(PET.in~yrmon+GCM,data=aggregate(PET.in~yrmon+GCM,data=WBData,sum),mean)[,3] #sum_pet -- conversion?
#MonthlyWB$avg_SM.in = aggregate(SM.in ~ yrmon+GCM, data=WBData, FUN=mean)[,3] #avg_soil -- conversion?
#MonthlyWB$sum_aet.in = aggregate(AET.in~yrmon+GCM,data=aggregate(AET.in~yrmon+GCM,data=WBData,sum),mean)[,3]#sum_aet -- conversion?
#MonthlyWB$sum_d.in = aggregate(D.in~yrmon+GCM,data=aggregate(D.in~yrmon+GCM,data=WBData,sum),mean)[,3] #sum_d -- conversion?
#
#AnnualWB$max_SWEaccum.in = aggregate(SWEaccum.in ~ Year+GCM, data=WBData, max)[,3]
#AnnualWB$sum_runoff.in = aggregate(Runoff.in ~ Year+GCM, data=aggregate(Runoff.in~Year+GCM,data=WBData,sum), mean)[,3]
#AnnualWB$sum_pet.in = aggregate(PET.in ~ Year+GCM, data=aggregate(PET.in~Year+GCM,data=WBData,sum), mean)[,3]
#AnnualWB$avg_SM.in = aggregate(SM.in ~ Year+GCM, data=WBData, FUN=mean)[,3]
#AnnualWB$sum_aet.in = aggregate(AET.in ~ Year+GCM, data=aggregate(AET.in~Year+GCM,data=WBData,sum), mean)[,3]
#AnnualWB$sum_d.in = aggregate(D.in ~ Year+GCM, data=aggregate(D.in~Year+GCM,data=WBData,sum), mean)[,3]

#MonthlyWB %>% mutate_at(3:9,funs(round(.,1))) %>% write.csv(.,paste0(TableDir,"WB-Monthly.csv"),row.names=FALSE)
#AnnualWB %>% mutate_at(3:9,funs(round(.,1))) %>% write.csv(.,paste0(TableDir,"WB-Annual.csv"),row.names=FALSE)



#######################################################################################################################
######################################### PLOTTING ####################################################################
# Inputs
MonthlyWB<-merge(MonthlyWB,WB_GCMs,by="GCM", all.x=T)
MonthlyWB$CF<-factor(MonthlyWB$CF, levels=c("Historical",CFs))
MonthlyWB <- MonthlyWB %>%  drop_na()

AnnualWB<-merge(AnnualWB,WB_GCMs,by="GCM", all.x=T)
AnnualWB$CF<-factor(AnnualWB$CF, levels=c("Historical",CFs))
AnnualWB <- AnnualWB %>% drop_na()

# Conversions to Imperial Units

#Annual Conversions
AnnualWB$sum_p.in <- (AnnualWB$sum_p.mm/ 25.4)
AnnualWB$avg_t.F <- (AnnualWB$avg_t.C * (9/5) + 32)
AnnualWB$sum_rain.in <- (AnnualWB$sum_rain.mm/ 25.4)
AnnualWB$sum_snow.in <- (AnnualWB$sum_snow.mm/ 25.4)
AnnualWB$max_pack.in <- (AnnualWB$max_pack.mm/ 25.4)
AnnualWB$sum_melt.in <- (AnnualWB$sum_melt.mm/ 25.4)
AnnualWB$sum_w.in <- (AnnualWB$sum_w.mm/ 25.4)
AnnualWB$sum_pet.in <- (AnnualWB$sum_pet.mm/ 25.4)
AnnualWB$sum_w_pet.in <- (AnnualWB$sum_w_pet.mm/ 25.4)
AnnualWB$avg_soil.in <- (AnnualWB$avg_soil.mm/ 25.4)
AnnualWB$sum_aet.in <- (AnnualWB$sum_aet.mm/ 25.4)
AnnualWB$runoff.in <- (AnnualWB$runoff.mm/ 25.4)
AnnualWB$sum_d.in <- (AnnualWB$sum_d.mm/ 25.4)
AnnualWB$sum_gdd.F <- (AnnualWB$sum_gdd.C * (9/5) + 32)

AnnualWB <- subset(AnnualWB, select = -c(3:16))

#Monthly Conversions
MonthlyWB$sum_p.in <- (MonthlyWB$sum_p.mm/ 25.4)
MonthlyWB$avg_t.F <- (MonthlyWB$avg_t.C * (9/5) + 32)
MonthlyWB$sum_rain.in <- (MonthlyWB$sum_rain.mm/ 25.4)
MonthlyWB$sum_snow.in <- (MonthlyWB$sum_snow.mm/ 25.4)
MonthlyWB$max_pack.in <- (MonthlyWB$max_pack.mm/ 25.4)
MonthlyWB$sum_melt.in <- (MonthlyWB$sum_melt.mm/ 25.4)
MonthlyWB$sum_w.in <- (MonthlyWB$sum_w.mm/ 25.4)
MonthlyWB$sum_pet.in <- (MonthlyWB$sum_pet.mm/ 25.4)
MonthlyWB$sum_w_pet.in <- (MonthlyWB$sum_w_pet.mm/ 25.4)
MonthlyWB$avg_soil.in <- (MonthlyWB$avg_soil.mm/ 25.4)
MonthlyWB$sum_aet.in <- (MonthlyWB$sum_aet.mm/ 25.4)
MonthlyWB$runoff.in <- (MonthlyWB$runoff.mm/ 25.4)
MonthlyWB$sum_d.in <- (MonthlyWB$sum_d.mm/ 25.4)
#MonthlyWB$sum_gdd.F <- (MonthlyWB$sum_gdd.C * (9/5) + 32)

MonthlyWB <- subset(MonthlyWB, select = -c(3:15))


ggplot(AnnualWB, aes(x=sum_d.in, y=sum_aet.in, colour=CF)) +  
  geom_point(size=3) +   
  geom_smooth(method="lm", se=FALSE, size=2) +
  scale_colour_manual("",values=col) +
  labs(
    y = "Annual Actual Evapotranspiration (in)",
    x = "Annual water deficit (in)",
    colour = "GCM",
    title = paste("Water Balance for ",SiteID,sep="")  
  ) + PlotTheme + theme(axis.title.x=element_text(size=18, vjust=0.5,  margin=margin(t=20, r=20, b=20, l=20)))

ggsave("WaterBalance.png", path = FigDir, width = PlotWidth, height = PlotHeight)

density_plot(AnnualWB, xvar=sum_d.in,cols=col,title=paste(SiteID,"Water Deficit for GCMs in", Yr,  "and Historical Period (", BasePeriod,")",sep=" "),
             xlab="Annual deficit (in)")
ggsave("sum_d.in-Density.png", path = FigDir, width = PlotWidth, height = PlotHeight)

density_plot(AnnualWB, xvar=avg_soil.in,cols=col,title=paste(SiteID,"Soil Moisture for GCMs in", Yr,  "and Historical Period (", BasePeriod,")",sep=" "),
             xlab="Annual soil moisture (in)")
ggsave("avg_SM.in-Density.png", path = FigDir, width = PlotWidth, height = PlotHeight)


### Monthly Plots
MonthlyWB$Month <- substr(MonthlyWB$yrmon, 5, 7)
MonthlyWB_mean <- aggregate(.~CF+Month, MonthlyWB[,3:18],mean)
MonthlyWB_H <- subset(MonthlyWB_mean, CF == "Historical")
MonthlyWB_delta = list()
split<-split(MonthlyWB_mean,MonthlyWB_mean$CF)
for(i in 1:length(split)){
  MD <- split[[i]]
  MD[,3:16] <- MD[,3:16] - MonthlyWB_H[,3:16]
  MonthlyWB_delta[[i]] <- MD ; rm(MD)
}
MonthlyWB_delta<- ldply(MonthlyWB_delta, data.frame)
MonthlyWB_delta <- subset(MonthlyWB_delta, CF %in% CFs)
MonthlyWB_delta$CF<-droplevels(MonthlyWB_delta$CF)


## avg_SM.in
Month_line_plot(MonthlyWB_delta, Month, avg_soil.in, grp=CF, cols=colors2, 
                title= paste("Change in average monthly soil moisture in", Yr, "vs Historical (",BasePeriod,")"),
                xlab="Month", ylab="Change in soil moisture (inches)")
ggsave("avg_SM.in-Monthly-line.png", path = FigDir, width = PlotWidth, height = PlotHeight)

dot_plot(MonthlyWB_delta, avg_soil.in, Month, grp=CF, cols=colors2,
         title = paste("Change in average monthly soil moisture in", Yr, "vs Historical (",BasePeriod,")"),
         xlab="Change in soil moisture (inches)",ylab="Month",labels=MonthLabels)
ggsave("avg_SM.in-Monthly-dot.png", path = FigDir, width = PlotWidth, height = PlotHeight)

## sum_d.in
Month_line_plot(MonthlyWB_delta, Month, sum_d.in, grp=CF, cols=colors2, 
                title= paste("Change in average monthly water deficit in", Yr, "vs Historical (",BasePeriod,")"),
                xlab="Month", ylab="Change in deficit (inches)")
ggsave("sum_d.in-Monthly-line.png", path = FigDir, width = PlotWidth, height = PlotHeight)

dot_plot(MonthlyWB_delta, sum_d.in, Month, grp=CF, cols=colors2,
         title = paste("Change in average monthly water deficit in", Yr, "vs Historical (",BasePeriod,")"),
         xlab="Change in deficit (inches)",ylab="Month",labels=MonthLabels)
ggsave("sum_d.in-Monthly-dot.png", path = FigDir, width = PlotWidth, height = PlotHeight)


## runoff.in
Month_line_plot(MonthlyWB_delta, Month, runoff.in, grp=CF, cols=colors2, 
                title= paste("Change in average monthly runoff in", Yr, "vs Historical (",BasePeriod,")"),
                xlab="Month", ylab="Change in runoff (inches)")
ggsave("sum_runoff.in-Monthly-line.png", path = FigDir, width = PlotWidth, height = PlotHeight)

dot_plot(MonthlyWB_delta, runoff.in, Month, grp=CF, cols=colors2,
         title = paste("Change in average monthly runoff in", Yr, "vs Historical (",BasePeriod,")"),
         xlab="Change in runoff (inches)",ylab="Month",labels=MonthLabels)
ggsave("sum_runoff.in-Monthly-dot.png", path = FigDir, width = PlotWidth, height = PlotHeight)


## max_pack.in
Month_line_plot(MonthlyWB_delta, Month, max_pack.in, grp=CF, cols=colors2, 
                title= paste("Change in average monthly SWE in", Yr, "vs Historical (",BasePeriod,")"),
                xlab="Month", ylab="Change in SWE (inches)")
ggsave("sum_SWEaccum.in-Monthly-line.png", path = FigDir, width = PlotWidth, height = PlotHeight)

dot_plot(MonthlyWB_delta, max_pack.in, Month, grp=CF, cols=colors2,
         title = paste("Change in average monthly SWE in", Yr, "vs Historical (",BasePeriod,")"),
         xlab="Change in SWE (inches)",ylab="Month",labels=MonthLabels)
ggsave("sum_SWEaccum.in-Monthly-dot.png", path = FigDir, width = PlotWidth, height = PlotHeight)


## sum_aet.in
Month_line_plot(MonthlyWB_delta, Month, sum_aet.in, grp=CF, cols=colors2, 
                title= paste("Change in average monthly AET in", Yr, "vs Historical (",BasePeriod,")"),
                xlab="Month", ylab="Change in AET (inches)")
ggsave("sum_aet.in-Monthly-line.png", path = FigDir, width = PlotWidth, height = PlotHeight)

dot_plot(MonthlyWB_delta, sum_aet.in, Month, grp=CF, cols=colors2,
         title = paste("Change in average monthly AET in", Yr, "vs Historical (",BasePeriod,")"),
         xlab="Change in AET (inches)",ylab="Month",labels=MonthLabels)
ggsave("sum_aet.in-Monthly-dot.png", path = FigDir, width = PlotWidth, height = PlotHeight)


### Additional plots
# Max SWE
# AnnualWB$max_SWEaccum.in <- aggregate(SWEaccum.in ~ Year+GCM, data=aggregate(SWEaccum.in~Year+GCM,data=WBData,sum), mean)[,3]
density_plot(AnnualWB, xvar=max_pack.in,cols=col,title=paste(SiteID,"maximum annual SWE in", Yr,  "and Historical Period (", BasePeriod,")",sep=" "),
             xlab="Max SWE (in)")
ggsave("SWEaccum.in-Density-max.png", path = FigDir, width = PlotWidth, height = PlotHeight)

var_bar_plot(AnnualWB, "max_pack.in", cols=colors3, ylab="Max SWE (in)",
             title=paste0("Average annual max SWE in ", Yr, " vs ", BasePeriod))
ggsave("max_pack.in-Annual-bar.png", width = PlotWidth, height = PlotHeight, path = FigDir)

var_line_plot(AnnualWB, var=max_pack.in, cols=col, title="Average annual max SWE in.",
              ylab="Max SWE (in)")
ggsave("max_SWEaccum.in-Annual-line.png", width = PlotWidth, height = PlotHeight, path = FigDir)


### Adjust water year for spaghetti plots
hydro.day.new = function(x, start.month = 10L){
  x <- as.Date(x)
  start.yr = year(x) - (month(x) < start.month)
  start.date = make_date(start.yr, start.month, 1L)
  as.integer(x - start.date + 1L)
}
WBData$WaterYr <- hydro.day.new(WBData$Date)

## Add CFs to WBData

WBData<-merge(WBData,WB_GCMs,by="GCM", all.x=T)
WBData$CF<-factor(WBData$CF, levels=c("Historical",CFs))
WBData <- WBData %>% drop_na()

# SWE spaghetti
Hist.SWE<-spaghetti_plot_wateryr(subset(WBData,CF=="Historical"),"SWEaccum.in",col=col[1],CF="Historical")
CF1.SWE<-spaghetti_plot_wateryr(subset(WBData,CF %in% CFs[1]),"SWEaccum.in",col=col[2], CF=CFs[1])
CF2.SWE<-spaghetti_plot_wateryr(subset(WBData,CF %in% CFs[2]),"SWEaccum.in",col=col[3], CF=CFs[2])

SWEgrid <- ggarrange(Hist.SWE, CF1.SWE, CF2.SWE, ncol = 1, nrow = 3,common.legend = T)

annotate_figure(SWEgrid, left = textGrob("SWE (in)", rot = 90, vjust = 1, gp = gpar(cex = 1.3)),
                bottom = textGrob("Water year day", gp = gpar(cex = 1.3)),
                top = textGrob("Daily SWE for each climate future by water year",
                               gp=gpar(fontface="bold", col="black",  fontsize=26)))
ggsave("SWEaccum.in-spaghetti.png", width = PlotWidth, height = PlotHeight, path = FigDir)


# runoff spaghetti


Hist.runoff<-spaghetti_plot_wateryr(subset(WBData,CF=="Historical"),"Runoff.in",col=col[1],CF="Historical")
CF1.runoff<-spaghetti_plot_wateryr(subset(WBData,CF %in% CFs[1]),"Runoff.in",col=col[2], CF=CFs[1])
CF2.runoff<-spaghetti_plot_wateryr(subset(WBData,CF %in% CFs[2]),"Runoff.in",col=col[3], CF=CFs[2])

runoffgrid <- ggarrange(Hist.runoff, CF1.runoff, CF2.runoff, ncol = 1, nrow = 3,common.legend = T)

annotate_figure(runoffgrid, left = textGrob("Runoff (in)", rot = 90, vjust = 1, gp = gpar(cex = 1.3)),
                bottom = textGrob("Water year day", gp = gpar(cex = 1.3)),
                top = textGrob("Daily Runoff for each climate futureby water year",
                               gp=gpar(fontface="bold", col="black",  fontsize=26)))
ggsave("Runoff.in-spaghetti.png", width = PlotWidth, height = PlotHeight, path = FigDir)

rm(Hist.SWE,CF1.SWE,CF2.SWE,SWEgrid,Hist.runoff,CF1.runoff,CF2.runoff, runoffgrid)

# aet spaghetti


Hist.AET<-spaghetti_plot_wateryr(subset(WBData,CF=="Historical"),"AET.in",col=col[1],CF="Historical")
CF1.AET<-spaghetti_plot_wateryr(subset(WBData,CF %in% CFs[1]),"AET.in",col=col[2], CF=CFs[1])
CF2.AET<-spaghetti_plot_wateryr(subset(WBData,CF %in% CFs[2]),"AET.in",col=col[3], CF=CFs[2])

aetgrid <- ggarrange(Hist.AET, CF1.AET, CF2.AET, ncol = 1, nrow = 3,common.legend = T)

annotate_figure(aetgrid, left = textGrob("AET (in)", rot = 90, vjust = 1, gp = gpar(cex = 1.3)),
                bottom = textGrob("Water year day", gp = gpar(cex = 1.3)),
                top = textGrob("Daily AET for each climate future by water year",
                               gp=gpar(fontface="bold", col="black",  fontsize=26)))
ggsave("AET.in-spaghetti.png", width = PlotWidth, height = PlotHeight, path = FigDir)


# SoilMoisture spaghetti


Hist.SM<-spaghetti_plot_wateryr(subset(WBData,CF=="Historical"),"SM.in",col=col[1],CF="Historical")
CF1.SM<-spaghetti_plot_wateryr(subset(WBData,CF %in% CFs[1]),"SM.in",col=col[2], CF=CFs[1])
CF2.SM<-spaghetti_plot_wateryr(subset(WBData,CF %in% CFs[2]),"SM.in",col=col[3], CF=CFs[2])

SMgrid <- ggarrange(Hist.SM, CF1.SM, CF2.SM, ncol = 1, nrow = 3,common.legend = T)

annotate_figure(aetgrid, left = textGrob("Soil Moisture (in)", rot = 90, vjust = 1, gp = gpar(cex = 1.3)),
                bottom = textGrob("Water year day", gp = gpar(cex = 1.3)),
                top = textGrob("Daily Soil Moisture for each climate future by water year",
                               gp=gpar(fontface="bold", col="black",  fontsize=26)))
ggsave("SM.in-spaghetti.png", width = PlotWidth, height = PlotHeight, path = FigDir)


rm(Hist.SWE,CF1.SWE,CF2.SWE,SWEgrid,Hist.runoff,CF1.runoff,CF2.runoff, runoffgrid,Hist.AET,CF1.AET,CF2.AET,aetgrid,Hist.SM,
   CF1.SM,CF2.SM,SMgrid)






