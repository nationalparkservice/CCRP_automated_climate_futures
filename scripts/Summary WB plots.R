# Working off the regular summary plots, perform bias correction, then run R WB model for PRISM and all GCMs in CFs using
# Plot

PARK<-SiteID
ALL_FUTURE<-merge(ALL_FUTURE,CF_GCM,by="GCM")

BC.min = 1979 #Bias correction range
BC.max = 2018 

#CSV file containing properties for all sites
n<-nrow(wb_sites)
#Threshold temperature (deg C) for growing degree-days calculation
T.Base = 0 

#Method for PET calculation 
# Method = "Hamon"  #Hamon is default method for daily PRISM and MACA data (containing only Tmax, Tmin, and Date). 
Method = "Thornthwaite"  #Thornthwaite is default method for monthly PRISM and MACA data (containing only Tmax, Tmin, and Date). 

#Date format
DateFormat = "%m/%d/%Y"

#Output directory
OutDir = './figures/summary-plots'

#Select GCMs - Include RCP
GCMs = unique(ALL_FUTURE$GCM[which(ALL_FUTURE$CF %in% CF.sub)]) 

############################################################ END USER INPUTS ###################################################################


############################################ Format Gridmet data ####################################################
head(grid)
grid$tmean<-(grid$tmax+grid$tmin)/2
grid$Date = as.Date(grid$Date, "%m/%d/%Y")
grid$month = strftime(grid$Date, "%m")
grid$year = strftime(grid$Date, "%Y")
grid.yrMAvgs = aggregate(tmean ~ year+month, data=grid, FUN=mean)

ppt.yrMAvgs = aggregate(precip~year+month, data=grid, FUN=sum)

grid.yrMAvgs = merge(grid.yrMAvgs,ppt.yrMAvgs,by="year")
grid.yrMAvgs$CF<-"Historical"


############################################ Format PRISM data #####################################################
PptMeans$month = strftime(PptMeans$Date, "%m")
PptMeans$year = strftime(PptMeans$Date, "%Y")
PRISM.tmean = data.frame(Date = TmaxMeans$Date, TavgCustom = (TmaxMeans$TmaxF + TminMeans$TminF)/2)
PRISM.tmean$year = strftime(PRISM.tmean$Date, "%Y")
PRISM.tmean$month = strftime(PRISM.tmean$Date, "%m")

PRISM.Avgs = merge(PRISM.tmean,PptMeans,by=c("year","month"))
PRISM.Avgs$year = as.numeric(as.character(PRISM.Avgs$year))
PRISM.Avgs$year = as.numeric(as.character(PRISM.Avgs$year))

########################################### Bias correction ########################################################
Grid.tmean = mean(grid.yrMAvgs$tmean[which(grid.yrMAvgs$year>=BC.min & grid.yrMAvgs$year<=BC.max)])
PRISM.tmean = mean(PRISM.Avgs$TavgCustom[which(PRISM.Avgs$year>=BC.min & PRISM.Avgs$year <=BC.max)])
BC.tmean = Grid.tmean - PRISM.tmean

Grid.ppt = mean(grid.yrMAvgs$precip[which(grid.yrMAvgs$year>=BC.min & grid.yrMAvgs$year<=BC.max)])
PRISM.ppt = mean(PRISM.Avgs$PptIn[which(PRISM.Avgs$year>=BC.min & PRISM.Avgs$year <=BC.max)])
BC.ppt = Grid.ppt - PRISM.ppt

#Bias-corrected values
PRISM.BC = data.frame(year = PRISM.Avgs$year, month = PRISM.Avgs$month,
                          CF = "Historical",
                          Tavg.mean = PRISM.Avgs$TavgCustom + BC.tmean,
                          Precip.mean = PRISM.Avgs$PptIn + BC.ppt)

########################################### Run WB for PRISM #######################################################
# Convert pr.In to mm and F to C
PRISM.BC$ppt_mm <- (PRISM.BC$Precip.mean*25.4)
PRISM.BC$tmean_C <- 5/9*(PRISM.BC$Tavg.mean -32)
PRISM.BC$Date<-as.Date(paste(PRISM.BC$year,PRISM.BC$month,"1",sep="-"),format="%Y-%m-%d")

AllMonthlyWB<-list()
for(i in 1:nrow(wb_sites)){
  SiteID = wb_sites$SiteID[i]
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
  
  MonthlyWB = PRISM.BC
  MonthlyWB$site = SiteID
  MonthlyWB$daylength = get_daylength(MonthlyWB$Date, Lat)
  MonthlyWB$jtemp = as.numeric(get_jtemp(Lon, Lat))
  MonthlyWB$F = get_freeze(MonthlyWB$jtemp, MonthlyWB$tmean_C)
  MonthlyWB$RAIN = get_rain(MonthlyWB$ppt_mm, MonthlyWB$F)
  MonthlyWB$SNOW = get_snow(MonthlyWB$ppt_mm, MonthlyWB$F)
  MonthlyWB$MELT = get_melt(MonthlyWB$tmean_C, MonthlyWB$jtemp, hock=4, MonthlyWB$SNOW, Snowpack.Init)
  MonthlyWB$PACK = get_snowpack(MonthlyWB$jtemp, MonthlyWB$SNOW, MonthlyWB$MELT)
  MonthlyWB$W = MonthlyWB$MELT + MonthlyWB$RAIN
  if(Method == "Thornthwaite"){
    MonthlyWB$PET = ET_Thorn_monthly(MonthlyWB)
  }
  MonthlyWB$PET = modify_PET(MonthlyWB$PET, Slope, Aspect, Lat, Shade.Coeff)
  MonthlyWB$W_PET = MonthlyWB$W - MonthlyWB$PET
  MonthlyWB$SOIL = get_soil(MonthlyWB$W, Soil.Init, MonthlyWB$PET, MonthlyWB$W_PET, SWC.Max)
  MonthlyWB$DSOIL = diff(c(Soil.Init, MonthlyWB$SOIL))
  MonthlyWB$AET = get_AET(MonthlyWB$W, MonthlyWB$PET, MonthlyWB$SOIL, Soil.Init)
  MonthlyWB$W_ET_DSOIL = MonthlyWB$W - MonthlyWB$AET - MonthlyWB$DSOIL
  MonthlyWB$D = MonthlyWB$PET - MonthlyWB$AET
  MonthlyWB$GDD = get_GDD(MonthlyWB$tmean_C, T.Base)
  AllMonthlyWB[[i]] = MonthlyWB
}
MonthlyWB<-do.call(rbind,AllMonthlyWB)
MonthlyWB<-aggregate(.~year+month+CF,MonthlyWB,mean)
############### AGGREGATE TO ANNUAL & CONVERT TO D TO INCHES ##########
MonthlyWB$Date <- as.POSIXct.Date(MonthlyWB$Date)
MonthlyWB$year = as.numeric(strftime(MonthlyWB$Date, "%Y"))

#Annual
PRISMWB = data.frame(year = unique(MonthlyWB$year))
PRISMWB$sum_p = aggregate(ppt_mm ~ year, data=MonthlyWB, FUN=sum)[,2]
PRISMWB$avg_t = aggregate(tmean_C ~ year, data=MonthlyWB, FUN=mean)[,2]
PRISMWB$sum_rain = aggregate(RAIN ~ year, data=MonthlyWB, FUN=sum)[,2]
PRISMWB$sum_snow = aggregate(SNOW ~ year, data=MonthlyWB, FUN=sum)[,2]
PRISMWB$max_pack = aggregate(PACK ~ year, data=MonthlyWB, FUN=max)[,2]
PRISMWB$sum_melt = aggregate(MELT ~ year, data=MonthlyWB, FUN=sum)[,2]
PRISMWB$sum_w = aggregate(W ~ year, data=MonthlyWB, FUN=sum)[,2]
PRISMWB$sum_pet = aggregate(PET ~ year, data=MonthlyWB, FUN=sum)[,2]
PRISMWB$sum_w_pet = aggregate(W_PET ~ year, data=MonthlyWB, FUN=sum)[,2]
PRISMWB$avg_soil = aggregate(SOIL ~ year, data=MonthlyWB, FUN=mean)[,2]
PRISMWB$sum_aet = aggregate(AET ~ year, data=MonthlyWB, FUN=sum)[,2]
PRISMWB$sum_w_et_dsoil = aggregate(W_ET_DSOIL ~ year, data=MonthlyWB, FUN=sum)[,2]
PRISMWB$sum_d = aggregate(D ~ year, data=MonthlyWB, FUN=sum)[,2]
PRISMWB$D_in<-PRISMWB$sum_d/25.4
  
######################################################### END PRISM WB CALCULATIONS ################################################################
  
 
########################################### Format MACA data #######################################################
ALL_FUTURE$month = strftime(ALL_FUTURE$Date, "%m")
ALL_FUTURE$year = strftime(ALL_FUTURE$Date, "%Y")
ALL_FUTURE<-subset(ALL_FUTURE, GCM %in% GCMs)

MACA.Avgs = aggregate(TavgCustom ~ year+month+GCM+CF, data=ALL_FUTURE, FUN=mean)
MACA.ppt = aggregate(PrecipCustom ~ year+month+GCM+CF, data=ALL_FUTURE, FUN=sum)
MACA.Avgs = merge(MACA.Avgs,MACA.ppt,by=c("year","month","GCM","CF"))
# Convert pr.In to mm and F to C
MACA.Avgs$Date<-as.Date(paste(MACA.Avgs$year,MACA.Avgs$month,"1",sep="-"),format="%Y-%m-%d")
MACA.Avgs$ppt_mm <- MACA.Avgs$PrecipCustom*25.4
MACA.Avgs$tmean_C<-5/9*(MACA.Avgs$TavgCustom - 32)

AllMonthlyWB<-list()

for (j in 1:length(GCMs)){
  gcm = GCMs[j]
  MonthlyWB = subset(MACA.Avgs,GCM == gcm)
  for(i in 1:nrow(wb_sites)){
    SiteID = wb_sites$WB_site[i]
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
    
    MonthlyWB$site = SiteID
    MonthlyWB$daylength = get_daylength(MonthlyWB$Date, Lat)
    MonthlyWB$jtemp = as.numeric(get_jtemp(Lon, Lat))
    MonthlyWB$F = get_freeze(MonthlyWB$jtemp, MonthlyWB$tmean_C)
    MonthlyWB$RAIN = get_rain(MonthlyWB$ppt_mm, MonthlyWB$F)
    MonthlyWB$SNOW = get_snow(MonthlyWB$ppt_mm, MonthlyWB$F)
    MonthlyWB$MELT = get_melt(MonthlyWB$tmean_C, MonthlyWB$jtemp, hock=4, MonthlyWB$SNOW, Snowpack.Init)
    MonthlyWB$PACK = get_snowpack(MonthlyWB$jtemp, MonthlyWB$SNOW, MonthlyWB$MELT)
    MonthlyWB$W = MonthlyWB$MELT + MonthlyWB$RAIN
    if(Method == "Thornthwaite"){
      MonthlyWB$PET = ET_Thorn_monthly(MonthlyWB)
    }
    MonthlyWB$PET = modify_PET(MonthlyWB$PET, Slope, Aspect, Lat, Shade.Coeff)
    MonthlyWB$W_PET = MonthlyWB$W - MonthlyWB$PET
    MonthlyWB$SOIL = get_soil(MonthlyWB$W, Soil.Init, MonthlyWB$PET, MonthlyWB$W_PET, SWC.Max)
    MonthlyWB$DSOIL = diff(c(Soil.Init, MonthlyWB$SOIL))
    MonthlyWB$AET = get_AET(MonthlyWB$W, MonthlyWB$PET, MonthlyWB$SOIL, Soil.Init)
    MonthlyWB$W_ET_DSOIL = MonthlyWB$W - MonthlyWB$AET - MonthlyWB$DSOIL
    MonthlyWB$D = MonthlyWB$PET - MonthlyWB$AET
    MonthlyWB$GDD = get_GDD(MonthlyWB$tmean_C, T.Base)
  }
  AllMonthlyWB[[j]] = MonthlyWB
}
WBData<-do.call(rbind,AllMonthlyWB)
WBData<-aggregate(.~year+month+GCM+CF,WBData,mean)
############### AGGREGATE TO ANNUAL & CONVERT TO D TO INCHES ##########
WBData$Date<-as.POSIXct.Date(WBData$Date)
WBData$year = as.numeric(strftime(WBData$Date, "%Y"))

#Annual
MACAWB = aggregate(ppt_mm ~ year+GCM+CF, data=WBData, FUN=sum)
MACAWB$avg_t = aggregate(tmean_C ~ year+GCM+CF, data=WBData, FUN=mean)[,4]
MACAWB$sum_rain = aggregate(RAIN ~ year+GCM+CF, data=WBData, FUN=sum)[,4]
MACAWB$sum_snow = aggregate(SNOW ~ year+GCM+CF, data=WBData, FUN=sum)[,4]
MACAWB$max_pack = aggregate(PACK ~ year+GCM+CF, data=WBData, FUN=max)[,4]
MACAWB$sum_melt = aggregate(MELT ~ year+GCM+CF, data=WBData, FUN=sum)[,4]
MACAWB$sum_w = aggregate(W ~ year+GCM+CF, data=WBData, FUN=sum)[,4]
MACAWB$sum_pet = aggregate(PET ~ year+GCM+CF, data=WBData, FUN=sum)[,4]
MACAWB$sum_w_pet = aggregate(W_PET ~ year+GCM+CF, data=WBData, FUN=sum)[,4]
MACAWB$avg_soil = aggregate(SOIL ~ year+GCM+CF, data=WBData, FUN=mean)[,4]
MACAWB$sum_aet = aggregate(AET ~ year+GCM+CF, data=WBData, FUN=sum)[,4]
MACAWB$sum_w_et_dsoil = aggregate(W_ET_DSOIL ~ year+GCM+CF, data=WBData, FUN=sum)[,4]
MACAWB$sum_d = aggregate(D ~ year+GCM+CF, data=WBData, FUN=sum)[,4]
MACAWB$D_in<-MACAWB$sum_d/25.4

########################################## CREATE PLOTTING DFS ###################################################################
PRISM.D<-data.frame(year=PRISMWB$year, CF="Historical", D.mean=PRISMWB$D_in, D.max=NA, D.min=NA)

MACA.D.mean = aggregate(D_in ~ year+CF, data=MACAWB, FUN=mean)
MACA.D.min = aggregate(D_in~ year+CF, data=MACAWB, FUN=min)
MACA.D.max = aggregate(D_in ~ year+CF, data=MACAWB, FUN=max)

MACA.yrAvgs = MACA.D.mean
MACA.yrAvgs = merge(MACA.yrAvgs, MACA.D.max, by=c("year", "CF"))
MACA.yrAvgs = merge(MACA.yrAvgs, MACA.D.min, by=c("year", "CF"))
colnames(MACA.yrAvgs) = c("year", "CF", "D.mean", "D.max", "D.min")

PRISM.D = subset(PRISM.D,year<2016)
MACA.yrAvgs = subset(MACA.yrAvgs, year > 2019)

yrAvgs = rbind(PRISM.D, MACA.yrAvgs)
yrAvgs.sub = subset(yrAvgs, CF %in% c("Historical",CF.sub))

yrAvgs.sub$CF <- factor(yrAvgs.sub$CF, levels = c("Historical", CFs))

# Rolling 10 year avg

deficit_avg = with(PRISM.D, tapply(D.mean, year, mean))

PRISM.D$D.mean.roll10 <- rollmean(PRISM.D$D.mean, rollLen, fill = NA, align = "right")

yrAvgs.sub <- merge(yrAvgs.sub, PRISM.D[,c("year", "D.mean.roll10")], by = "year", all = TRUE)

############################################# Plotting ###########################################################
PlotTheme = theme(axis.text=element_text(size=20),    #Text size for axis tick mark labels
                  axis.title.x=element_text(size=24, hjust=0.5, margin=margin(t=20, r=20, b=20, l=20)),               #Text size and alignment for x-axis label
                  axis.title.y=element_text(size=24, vjust=0.5,  margin=margin(t=20, r=20, b=20, l=20)),              #Text size and alignment for y-axis label
                  plot.title=element_blank(),      #No title
                  legend.title=element_text(size=24),                                                                    #Text size of legend category labels
                  legend.text=element_text(size=20), #Text size of legend title
                  legend.position = c(0,1),legend.justification = c(-.1,1.1),  #Set top left
                  panel.border = element_blank(), #Remove border around plot
                  axis.line = element_line(colour = "black"), #Add axis lines
                  panel.background = element_blank(), #Background white
                  panel.grid.major = element_line("light grey",0.3)) #add grid back
#Height and width 
PlotWidth = 15
PlotHeight = 9

# Deficit
ggplot(yrAvgs.sub, aes(x=as.numeric(as.character(year)), y=D.mean, col=CF, fill=CF)) + 
  geom_rect(xmin=2025, xmax=2055, ymin=0, ymax=80, alpha=0.1, fill="lightgray", col="lightgray") +
  geom_ribbon(aes(x=as.numeric(as.character(year)), ymin=D.min, ymax=D.max, fill=CF), alpha=0.5) +
  geom_line(size=2) + geom_point(col="black", size=2, shape=16) +
  geom_point() +
  geom_line(aes(x=as.numeric(as.character(year)), y=D.mean.roll10),size=1.5,colour="gray37",na.rm=TRUE) +
  scale_x_continuous(breaks=c(1900, 1920, 1940, 1960, 1980, 2000, 2020, 2040, 2060, 2080, 2100)) +
  labs(x="Year", y="Mean annual climatic water deficit (in/year)") +
  scale_color_manual(name="Climate Future",values=col) +
  scale_fill_manual(name="Climate Future",values=col) + PlotTheme
ggsave(paste(PARK,"-Deficit.png",sep=""), path = './figures/summary-plots', height=PlotHeight, width=PlotWidth)


