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


#######################################################################################################################
######################################### PLOTTING ####################################################################
# Inputs
F.start = (Yr - (Range/2))
F.end = (Yr + (Range/2))

GCMsHist_plot <- WB_GCMs %>% 
  add_row(GCM = unique(Gridmet$GCM),
          CF = "Historical")
  
MonthlyWB<-merge(MonthlyWB,GCMsHist_plot,by="GCM")
AnnualWB<-merge(AnnualWB,GCMsHist_plot,by="GCM")

# Convert to Imperial units
AnnualWB$deficit<-AnnualWB$sum_d * 0.039
AnnualWB$AET<-AnnualWB$sum_aet * 0.039
AnnualWB$P<-AnnualWB$sum_p * 0.039
AnnualWB$Tmean<-(AnnualWB$avg_t * 9/5) + 32
AnnualWB$SOIL_in<-AnnualWB$avg_soil

# Subset to correct years for each period
H_annual<-subset(AnnualWB,year<2000,select=c("year","CF","deficit","AET","P","Tmean","SOIL_in","sum_d","sum_aet"))
F_annual<-subset(AnnualWB,year>=F.start & year<=F.end,select=c("year","CF","deficit","AET","P","Tmean","SOIL_in","sum_d","sum_aet"))
H_annual$CF<-"Historical"

# Average over sites and subset historical to be 30 years
Hist<-aggregate(cbind(deficit,AET,P,Tmean,SOIL_in,sum_d,sum_aet)~year+CF,mean,data=H_annual,na.rm=TRUE)
Fut<-aggregate(cbind(deficit,AET,P,Tmean,SOIL_in,sum_d,sum_aet)~year+CF,mean,data=F_annual,na.rm=TRUE)


Annual<-rbind(Hist,Fut)
Annual$CF = factor(Annual$CF, levels = c("Historical",CFs))

ggplot(Annual, aes(x=deficit, y=AET, colour=CF)) + geom_point(size=3)+ geom_smooth(method="lm", se=FALSE, size=2)+
  
  scale_colour_manual("Scenario",values=colors3) +
  labs(
    y = "Annual Actual Evapotranspiration (in)",
    x = "Annual moisture deficit (in)",
    colour = "GCM",
    title = paste("Water Balance for ",SiteID,sep="")  
  ) + theme(plot.title = element_text(hjust = 0.5)) + #+ geom_vline(xintercept=mean(Historical.wb$deficit), colour="black") +geom_vline(xintercept=mean(Future.wb$deficit), colour="blue")
  # size is pts
  theme(axis.text = element_text(size=20), axis.title = element_text(size=20), legend.text=element_text(size=14),
        plot.title=element_text(size=22)) #+xlim(20,45)+ylim(2,16)

ggsave(paste("Water Balance-",SiteID,".png",sep=""), path = WBdir, width = 15, height = 9)

ggplot(Annual, aes(x=deficit, colour=CF,fill=CF,linetype=CF),show.legend=F) +geom_density(alpha=0.3,size=1.5) +
  scale_colour_manual(values=colors3) +
  scale_fill_manual(values=colors3) +  
  scale_linetype_manual(values=seq(1,length(unique(Annual$CF)),1)) +
  labs(y = "Density",
       x = "Annual moisture deficit (in)",
       title = paste(SiteID,"  Water Deficit for GCMs (2025-2055) and Historical Period (1950-1999)",sep=" ")) +
  theme(axis.text = element_text(size=20), axis.title = element_text(size=20), legend.text=element_text(size=20), legend.background=element_rect(fill = "White", size = 0.5),
        plot.title=element_text(size=22, hjust=0),legend.position = c(.8,.8)) 

ggsave(paste(SiteID,"-Deficit_density_panel.png",sep=""), path = WBdir, width = 15, height = 9)

ggplot(Annual, aes(x=SOIL_in, colour=CF,fill=CF,linetype=CF),show.legend=F) +geom_density(alpha=0.3,size=1.5) +
  scale_colour_manual(values=colors3) +
  scale_fill_manual(values=colors3) +  
  scale_linetype_manual(values=seq(1,length(unique(Annual$CF)),1)) +
  labs(y = "Density",
       x = "Annual soil moisture (in)",
       title = paste(SiteID,"  Soil Moisture for GCMs (2025-2055) and Historical Period (1950-1999)",sep=" ")) +
  theme(axis.text = element_text(size=20), axis.title = element_text(size=20), legend.text=element_text(size=14),
        plot.title=element_text(size=22, hjust=0),legend.position = c(.8,.8)) 

ggsave(paste(SiteID,"-SOIL_in_density_panel.png",sep=""), path = WBdir, width = 15, height = 9)


########################
# biome plots
biome<-readRDS("./data/general/D_AET.Rds")
head(biome)
color<-as.character(unique(biome$color,ordered=T))

color<-setNames(as.character(unique(biome$color)),as.character(unique(biome$biome)))
color

PlotTheme = theme(axis.text=element_text(size=20),    #Text size for axis tick mark labels
                  axis.title.x=element_text(size=24, hjust=0.5, margin=margin(t=20, r=20, b=20, l=20)),               #Text size and alignment for x-axis label
                  axis.title.y=element_text(size=24, vjust=0.5,  margin=margin(t=20, r=20, b=20, l=20)),              #Text size and alignment for y-axis label
                  plot.title=element_text(size=26,face="bold",hjust=0.5, margin=margin(t=20, r=20, b=20, l=20)),      #Text size and alignment for plot title
                  legend.title=element_text(size=24),                                                                    #Text size of legend category labels
                  legend.text=element_text(size=20),                                                                   #Text size of legend title
                  legend.position = "right")         

plot_1 <- ggplot() +
  # add biome polygons
  geom_polygon(data = biome,
               aes(x    = D_mm,
                   y    = AET_mm,
                   fill = biome),
               # adjust polygon borders
               colour = "black",
               size   = 1) + PlotTheme +
  scale_fill_manual(name   = "Veg biomes",
                    breaks = names(color),
                    labels = names(color),
                    values = color) + 
  labs(title=paste(SiteID, " water balance effects on biome",sep=""),
       y = "Annual Evapotranspiration (mm)", x = "Annual moisture deficit (mm)") 
plot_1

plot_1 + geom_point(data=Annual, aes(x=sum_d, y=sum_aet, colour=CF), size=3) + 
  geom_smooth(data=Annual, aes(x=sum_d, y=sum_aet, colour=CF),method="lm", se=FALSE, size=2) + 
  scale_colour_manual("Scenario",values=colors3)
ggsave(paste(SiteID,"-WB-biome effects.png",sep=""), path = WBdir, width = 15, height = 9)

### Monthly
MonthlyWB$year<-as.numeric(substr(MonthlyWB$yrmon, 1, 4))
MonthlyWB$Month<-as.numeric(substr(MonthlyWB$yrmon, 5, 6))
MonthlyWB$SOIL_in<-MonthlyWB$avg_soil * 0.39
MonthlyWB$deficit<-MonthlyWB$sum_d * 0.39

H_monthly<-subset(MonthlyWB,year<2000)
F_monthly<-subset(MonthlyWB,year>=F.start & year<=F.end)
H_monthly$CF<-"Historical"

set.seed(50) # set seed so same every time
MHist.subset<-sample_n(H_monthly,size=length(F_monthly$CF)/length(unique(F_monthly$CF)),replace=F)

Mon<-aggregate(cbind(SOIL_in,deficit)~Month+CF,mean,data=MHist.subset,na.rm=TRUE)
Fut<-aggregate(cbind(SOIL_in,deficit)~Month+CF,mean,data=F_monthly,na.rm=TRUE)
Monthly<-rbind(Mon,Fut)
Monthly$CF = factor(Monthly$CF)

data<-Fut[,1:2]
data$SOIL_IN<-Fut$SOIL_in-Mon$SOIL_in
data$deficit<-Fut$deficit-Mon$deficit
data$mon<-month.abb[data$Month]
data$CF<-factor(data$CF, levels = c(CFs))

ggplot(data, aes(x=mon, y=SOIL_IN, group=CF, colour = CF)) +
  geom_line(colour = "black",size=2.5, stat = "identity") + # adds black outline
  geom_line(size = 2, stat = "identity") + 
  geom_point(colour = "black", size = 4, aes(fill = factor(CF), shape = factor(CF))) +
  theme(axis.text=element_text(size=16),
        axis.title.x=element_text(size=16,vjust=-0.2),
        axis.title.y=element_text(size=20,vjust=1.0),
        plot.title=element_text(size=24,face="bold",vjust=2,hjust=0.5),
        legend.text=element_text(size=20)) +
  scale_x_discrete(labels=month.abb) +
  labs(title = "Change in average monthly soil moisture \n 2040 (2025-2055) vs 1950-1999",
       x = "Month", y = "Change in soil moisture (inches)") +
  scale_color_manual(name="",values = colors2) +
  scale_fill_manual(name="",values = colors2) +
  scale_shape_manual(name="",values = c(21,22))

ggsave("MonthlySoil Moisture.png", path = WBdir, width = 15, height = 9)

ggplot(data, aes(x=mon, y=deficit, group=CF, colour = CF)) +
  geom_line(colour = "black",size=2.5, stat = "identity") + # adds black outline
  geom_line(size = 2, stat = "identity") + 
  geom_point(colour = "black", size = 4, aes(fill = factor(CF), shape = factor(CF))) +
  theme(axis.text=element_text(size=16),
        axis.title.x=element_text(size=16,vjust=-0.2),
        axis.title.y=element_text(size=20,vjust=1.0),
        plot.title=element_text(size=24,face="bold",vjust=2,hjust=0.5),
        legend.text=element_text(size=20)) +
  scale_x_discrete(labels=month.abb) +
  labs(title = "Change in average monthly water deficit \n 2040 (2025-2055) vs 1950-1999",
       x = "Month", y = "Change in deficit (inches)") +
  scale_color_manual(name="",values = colors2) +
  scale_fill_manual(name="",values = colors2) +
  scale_shape_manual(name="",values = c(21,22))

ggsave("Monthly Deficit.png", path = WBdir, width = 15, height = 9)


