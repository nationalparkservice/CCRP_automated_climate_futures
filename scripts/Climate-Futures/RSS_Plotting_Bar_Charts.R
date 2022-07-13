# RSS_Plotting_Bar_Charts.R
DataFile <- list.files(path = DataDir, pattern = 'Final_Environment.RData', full.names = TRUE) # Environment needs to be added if not parsing MACA data
load(DataFile)

vars <- names(F_annual[,4:length(F_annual)])

#################################################### SUBSET DATAFRAMES ###################################################
Monthly<-subset(Monthly,CF %in% FutureSubset); Monthly$CF<-factor(Monthly$CF, levels=c("Historical",FutureSubset))
# Monthly$Month<-factor(Monthly$Month,levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))


Monthly_delta<-subset(Monthly_delta,CF %in% FutureSubset); Monthly_delta$CF<-factor(Monthly_delta$CF, levels=c(FutureSubset))
# Monthly_delta$Month<-factor(Monthly_delta$Month,levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

Season<-subset(Season,CF %in% FutureSubset); Season$CF<-factor(Season$CF, levels=c("Historical",FutureSubset))
Season$season<-factor(Season$season,levels=c("Winter","Spring","Summer","Fall"))

Season_delta<-subset(Season_delta,CF %in% FutureSubset); Season_delta$CF<-factor(Season_delta$CF, levels=c(FutureSubset))
Season_delta$season<-factor(Season_delta$season,levels=c("Winter","Spring","Summer","Fall"))

# Join historical and future for bar plots
H <- subset(H_annual, select = -c(GCM))
F_annual<-subset(F_annual, CF %in% FutureSubset)
# Fut_annual <- F_annual %>% select(-GCM) %>% group_by(Year,CF) %>% 
#   summarise_all(mean) #drops NAs

Fut_annual<-aggregate(.~Year+CF,subset(F_annual, select = -c(GCM)),mean)

Annual<-rbind(H, Fut_annual)
Annual$CF<-factor(Annual$CF,levels=c("Historical",CFs), ordered=is.ordered(Annual$CF))

MACA_avgPr <- mean(H$PrcpIn)

### Add column to change color of CF1
Annual$me.col<-"b"
Annual$me.col[which(Annual$CF=="Hot Wet")]<-"w" #If not using HW, will get error


############################################################### Begin output plots ############################################
## Quadrant means scatterplots

#### quadrant means
dualscatter = ggplot(Future_Means, aes(DeltaTavg, DeltaPr*365, xmin=Tavg25, xmax=Tavg75, ymin=Pr25*365, ymax=Pr75*365))

dualscatter  + geom_text_repel(aes(label=GCM)) +
  geom_point(colour="black",size=4) +
  geom_point(aes(x=mean(DeltaTavg[which(CF==CFs[1])]), y=mean(365*DeltaPr[which(CF==CFs[1])])), shape=8, size=7, stroke=3, colour=colors2[1]) +
  geom_point(aes(x=mean(DeltaTavg[which(CF==CFs[2])]), y=mean(365*DeltaPr[which(CF==CFs[2])])), shape=8, size=7, stroke=3, colour=colors2[2]) +
  theme(axis.text=element_text(size=18),
        axis.title.x=element_text(size=18,vjust=-0.2),
        axis.title.y=element_text(size=18,vjust=0.2),
        plot.title=element_text(size=18,face="bold",vjust=2,hjust=0.5),
        legend.text=element_text(size=18), legend.title=element_text(size=16)) + 
  ###
  labs(title =paste(SiteID," Changes in climate means in ", Yr, " by GCM run",sep=""), 
       x = "Changes in annual average temperature (\u00B0F)", # Change
       y = "Changes in annual average precipitation (in)") + #change
  scale_color_manual(name="Scenarios", values=c("black")) +
  # scale_fill_manual(name="Scenarios",values = c("black")) + 
  theme(legend.position="none") +
  geom_rect(color = "black", alpha=0) + 
  geom_hline(aes(yintercept=mean(DeltaPr*365)),linetype=2) + #change
  geom_vline(aes(xintercept=mean(DeltaTavg)),linetype=2) #change

ggsave("scatter-CFmeansStar.png", width = PlotWidth, height = PlotHeight, path = FigDir)

#### quadrant means + WB selected models
dualscatter = ggplot(Future_Means, aes(DeltaTavg, DeltaPr*365, xmin=Tavg25, xmax=Tavg75, ymin=Pr25*365, ymax=Pr75*365))

dualscatter  + geom_text_repel(aes(label=GCM)) +
  geom_point(colour="black",size=4) +
  geom_point(aes(x=mean(DeltaTavg[which(CF==CFs[1])]), y=mean(365*DeltaPr[which(CF==CFs[1])])), shape=8, size=7, stroke=3, colour=colors2[1]) +
  geom_point(aes(x=mean(DeltaTavg[which(CF==CFs[2])]), y=mean(365*DeltaPr[which(CF==CFs[2])])), shape=8, size=7, stroke=3, colour=colors2[2]) +
  geom_point(aes(x=mean(DeltaTavg[which(WB_GCM==CFs[1])]), y=mean(365*DeltaPr[which(WB_GCM==CFs[1])])), shape=21, size=10, stroke=3, colour=colors2[1]) +
  geom_point(aes(x=mean(DeltaTavg[which(WB_GCM==CFs[2])]), y=mean(365*DeltaPr[which(WB_GCM==CFs[2])])), shape=21, size=10, stroke=3, colour=colors2[2]) +
  geom_point(aes(x=mean(DeltaTavg[which(WB_GCM==CFs[1])]), y=mean(365*DeltaPr[which(WB_GCM==CFs[1])])), shape=20, size=2,  colour=colors2[1]) +
  geom_point(aes(x=mean(DeltaTavg[which(WB_GCM==CFs[2])]), y=mean(365*DeltaPr[which(WB_GCM==CFs[2])])), shape=20, size=2,  colour=colors2[2]) +
  theme(axis.text=element_text(size=18),
        axis.title.x=element_text(size=18,vjust=-0.2),
        axis.title.y=element_text(size=18,vjust=0.2),
        plot.title=element_text(size=18,face="bold",vjust=2,hjust=0.5),
        legend.text=element_text(size=18), legend.title=element_text(size=16)) + 
  ###
  labs(title =paste(SiteID," Changes in climate means in ", Yr, " by GCM run",sep=""), 
       x = "Changes in annual average temperature (\u00B0F)", # Change
       y = "Changes in annual average precipitation (in)") + #change
  scale_color_manual(name="Scenarios", values=c("black")) +
  # scale_fill_manual(name="Scenarios",values = c("black")) + 
  theme(legend.position="none") +
  geom_rect(color = "black", alpha=0) + 
  geom_hline(aes(yintercept=mean(DeltaPr*365)),linetype=2) + #change
  geom_vline(aes(xintercept=mean(DeltaTavg)),linetype=2) #change

ggsave("scatter-CFmeansStar-IndivCircled.png", width = PlotWidth, height = PlotHeight, path = FigDir)


################################## Monthly/Seasonal delta plots #####################################

Month_line_plot(Monthly_delta,xvar=Month,yvar=TavgF,grp=CF,cols=colors2,
                title=paste0("Change in avg daily temperature in ",Yr," vs ", BasePeriod),
                xlab = "Month", ylab="Change in temperature (\u00B0F)")
ggsave("Monthly-line-TavgFDelta.png", width = PlotWidth, height = PlotHeight, path = FigDir)

Month_line_plot(Monthly_delta,xvar=Month,yvar=TminF,grp=CF,cols=colors2,
                title=paste0("Change in avg daily min temperature in ",Yr," vs ", BasePeriod),
                xlab = "Month", ylab="Change in temperature (\u00B0F)")
ggsave("Monthly-line-TminFDelta.png", width = PlotWidth, height = PlotHeight, path = FigDir)

Month_line_plot(Monthly_delta,xvar=Month,yvar=TmaxF,grp=CF,cols=colors2,
                title=paste0("Change in avg daily max temperature in ",Yr," vs ", BasePeriod),
                xlab = "Month", ylab="Change in temperature (\u00B0F)")
ggsave("Monthly-line-TmaxFDelta.png", width = PlotWidth, height = PlotHeight, path = FigDir)

Month_bar_plot(Monthly_delta,xvar=Month,yvar=PrcpIn,grp=CF,cols=colors2,
               title=paste0("Change in avg monthly precipitation in ",Yr," vs ", BasePeriod),
               xlab = "Month", ylab="Change in precipitation (in)",label=MonthLabels)
ggsave("Monthly-bar-PrcpInDelta.png", width = PlotWidth, height = PlotHeight, path = FigDir)

Month_bar_plot(Season_delta,xvar=season,yvar=PrcpIn,grp=CF,cols=colors2,
               title=paste0("Change in avg seasonal precipitation in ",Yr," vs ", BasePeriod),
               xlab = "Season", ylab="Change in precipitation (in)",label=SeasonLabels)
ggsave("Seasonal-bar-PrcpInDelta.png", width = PlotWidth, height = PlotHeight, path = FigDir)

Month_bar_plot(Monthly_delta,xvar=Month,yvar=RHmean,grp=CF,cols=colors2,
               title=paste0("Change in avg monthly relative humidity in ",Yr," vs ", BasePeriod),
               xlab = "Month", ylab="Change in relative humidity (%)",label=MonthLabels)
ggsave("Monthly-bar-RHmeanDelta.png", width = PlotWidth, height = PlotHeight, path = FigDir)

Month_bar_plot(Season_delta,xvar=season,yvar=RHmean,grp=CF,cols=colors2,
               title=paste0("Change in avg seasonal relative humidity in ",Yr," vs ", BasePeriod),
               xlab = "Season", ylab="Change in relative humidity (%)",label=SeasonLabels)
ggsave("Seasonal-bar-RHmeanDelta.png", width = PlotWidth, height = PlotHeight, path = FigDir)


############################################### ANNUAL TMEAN AND PRECIP BAR PLOTS ######################################################################

## TavgF
var_bar_plot(Annual, "TavgF", cols=colors3, ylab="(\u00B0F)",
             title=paste0("Average annual temperature (\u00B0F) in ", Yr, " vs ", BasePeriod))
ggsave("Annual-bar-TavgF.png", width = PlotWidth, height = PlotHeight, path = FigDir)

var_box_plot(Annual, "TavgF", cols=colors3, ylab="(\u00B0F)",
             title=paste0("Average annual temperature (\u00B0F) in ", Yr, " vs ", BasePeriod))
ggsave("Annual-box-TavgF.png", width = PlotWidth, height = PlotHeight, path = FigDir)

var_line_plot(Annual, var=TavgF, cols=col, title=paste0("Average annual temperature (\u00B0F)"),
              ylab="(\u00B0F)")
ggsave("Annual-line-TavgF.png", width = PlotWidth, height = PlotHeight, path = FigDir)


## PrcpIn
var_bar_plot(Annual, "PrcpIn", cols=colors3, ylab="inches/Yr",
             title=paste0("Average annual precipitation (in) in ", Yr, " vs ", BasePeriod))
ggsave("Annual-bar-PrcpIn.png", width = PlotWidth, height = PlotHeight, path = FigDir)

var_box_plot(Annual, "PrcpIn", cols=colors3, ylab="inches/Yr",
             title=paste0("Average annual precipitation (in) in ", Yr, " vs ", BasePeriod))
ggsave("Annual-box-PrcpIn.png", width = PlotWidth, height = PlotHeight, path = FigDir)

var_line_plot(Annual, var=PrcpIn, cols=col, title=paste0("Average annual precipitation (in)"),
              ylab="inches/Yr")
ggsave("Annual-line-PrcpIn.png", width = PlotWidth, height = PlotHeight, path = FigDir)


############################################### THRESHOLD BAR PLOTS ######################################################################

## OverHotTemp
var_bar_plot(Annual, "OverHotTemp", cols=colors3, ylab="Days/Yr",
             title=paste0("Average Days/Yr > ", HotTemp, " (\u00B0F) in ", Yr, " vs ", BasePeriod))
ggsave("Annual-bar-OverHotTemp.png", width = PlotWidth, height = PlotHeight, path = FigDir)

var_box_plot(Annual, "OverHotTemp", cols=colors3, ylab="Days/Yr",
             title=paste0("Average Days/Yr > ", HotTemp, " (\u00B0F) in ", Yr, " vs ", BasePeriod))
ggsave("Annual-box-OverHotTemp.png", width = PlotWidth, height = PlotHeight, path = FigDir)

var_line_plot(Annual, var=OverHotTemp, cols=col, title=paste0("Average Days/Yr > ", HotTemp, " (\u00B0F)"),
                                                              ylab="Days/Yr")
ggsave("Annual-line-OverHotTemp.png", width = PlotWidth, height = PlotHeight, path = FigDir)


## UnderColdTemp
var_bar_plot(Annual, "UnderColdTemp", cols=colors3, ylab="Days/Yr",
             title=paste0("Average Days/Yr < ", ColdTemp, " (\u00B0F) in ", Yr, " vs ", BasePeriod))
ggsave("Annual-bar-UnderColdTemp.png", width = PlotWidth, height = PlotHeight, path = FigDir)

var_box_plot(Annual, "UnderColdTemp", cols=colors3, ylab="Days/Yr",
             title=paste0("Average Days/Yr < ", ColdTemp, " (\u00B0F) in ", Yr, " vs ", BasePeriod))
ggsave("Annual-box-UnderColdTemp.png", width = PlotWidth, height = PlotHeight, path = FigDir)

var_line_plot(Annual, var=UnderColdTemp, cols=col, title=paste0("Average Days/Yr < ", ColdTemp, " (\u00B0F)"),
              ylab="Days/Yr")
ggsave("Annual-line-UnderColdTemp.png", width = PlotWidth, height = PlotHeight, path = FigDir)


## UnderLowQ
var_bar_plot(Annual, "UnderLowQ", cols=colors3, ylab="Days/Yr",
             title=paste0("Average Days/Yr < Historical 5th Percentile (", round(HistTminLow, 1), "\u00B0F) in ", Yr, " vs ", BasePeriod))
ggsave("Annual-bar-UnderLowQ.png", width = PlotWidth, height = PlotHeight, path = FigDir)

var_box_plot(Annual, "UnderLowQ", cols=colors3, ylab="Days/Yr",
             title=paste0("Average Days/Yr < Historical 5th Percentile (", round(HistTminLow, 1), "\u00B0F) in ", Yr, " vs ", BasePeriod))
ggsave("Annual-box-UnderLowQ.png", width = PlotWidth, height = PlotHeight, path = FigDir)

var_line_plot(Annual, var=UnderLowQ, cols=col, title=paste0("Average Days/Yr < Historical 5th Percentile (", round(HistTminLow, 1), "\u00B0F)"),
              ylab="Days/Yr")
ggsave("Annual-line-UnderLowQ.png", width = PlotWidth, height = PlotHeight, path = FigDir)


## OverHighQ
var_bar_plot(Annual, "OverHighQ", cols=colors3, ylab="Days/Yr",
             title=paste0("Average Days/Yr > Historical 95th Percentile (", round(HistTmaxHigh, 1), "\u00B0F) in ", Yr, " vs ", BasePeriod))
ggsave("Annual-bar-OverHighQ.png", width = PlotWidth, height = PlotHeight, path = FigDir)

var_box_plot(Annual, "OverHighQ", cols=colors3, ylab="Days/Yr",
             title=paste0("Average Days/Yr > Historical 95th Percentile (", round(HistTmaxHigh, 1), "\u00B0F) in ", Yr, " vs ", BasePeriod))
ggsave("Annual-box-OverHighQ.png", width = PlotWidth, height = PlotHeight, path = FigDir)

var_line_plot(Annual, var=OverHighQ, cols=col, title=paste0("Average Days/Yr > Historical 95th Percentile (", round(HistTmaxHigh, 1), "\u00B0F)"),
              ylab="Days/Yr")
ggsave("Annual-line-OverHighQ.png", width = PlotWidth, height = PlotHeight, path = FigDir)


## OverPrecip95
var_bar_plot(Annual, "OverPrecip95", cols=colors3, ylab="Days/Yr",
             title=paste0("Average Days/Yr Precipitation > Historical 95th Percentile (", round(HistPrecip95, 1), " in) \n in ", Yr, " vs ", BasePeriod))
ggsave("Annual-bar-OverPrecip95.png", width = PlotWidth, height = PlotHeight, path = FigDir)

var_box_plot(Annual, "OverPrecip95", cols=colors3, ylab="Days/Yr",
             title=paste0("Average Days/Yr Precipitation > Historical 95th Percentile (", round(HistPrecip95, 1), " in) in ", Yr, " vs ", BasePeriod))
ggsave("Annual-box-OverPrecip95.png", width = PlotWidth, height = PlotHeight, path = FigDir)

var_line_plot(Annual, var=OverPrecip95, cols=col, title=paste0("Average Days/Yr Precipitation > Historical 95th Percentile (", round(HistPrecip95, 1), " in)"),
              ylab="Days/Yr")
ggsave("Annual-line-OverPrecip95.png", width = PlotWidth, height = PlotHeight, path = FigDir)


## PrecipOver1
var_bar_plot(Annual, "PrecipOver1", cols=colors3, ylab="Days/Yr",
             title=paste0("Average Days/Yr Precipitation > 1 in. in ", Yr, " vs ", BasePeriod))
ggsave("Annual-bar-PrecipOver1.png", width = PlotWidth, height = PlotHeight, path = FigDir)

var_box_plot(Annual, "PrecipOver1", cols=colors3, ylab="Days/Yr",
             title=paste0("Average Days/Yr Precipitation > 1 in. in ", Yr, " vs ", BasePeriod))
ggsave("Annual-box-PrecipOver1.png", width = PlotWidth, height = PlotHeight, path = FigDir)

var_line_plot(Annual, var=PrecipOver1, cols=col, title=paste0("Average Days/Yr Precipitation > 1 in."),
              ylab="Days/Yr")
ggsave("Annual-line-PrecipOver1.png", width = PlotWidth, height = PlotHeight, path = FigDir)


## PrecipOver2
var_bar_plot(Annual, "PrecipOver2", cols=colors3, ylab="Days/Yr",
             title=paste0("Average Days/Yr Precipitation > 2 in. in ", Yr, " vs ", BasePeriod))
ggsave("Annual-bar-PrecipOver2.png", width = PlotWidth, height = PlotHeight, path = FigDir)

var_box_plot(Annual, "PrecipOver2", cols=colors3, ylab="Days/Yr",
             title=paste0("Average Days/Yr Precipitation > 2 in. in ", Yr, " vs ", BasePeriod))
ggsave("Annual-box-PrecipOver2.png", width = PlotWidth, height = PlotHeight, path = FigDir)

var_line_plot(Annual, var=PrecipOver2, cols=col, title=paste0("Average Days/Yr Precipitation > 2 in."),
              ylab="Days/Yr")
ggsave("Annual-line-PrecipOver2.png", width = PlotWidth, height = PlotHeight, path = FigDir)


## FThaw
var_bar_plot(Annual, "FThaw", cols=colors3, ylab="Cycles/Yr",
             title=paste0("Average annual freeze-thaw cycles in ", Yr, " vs ", BasePeriod))
ggsave("Annual-bar-FThaw.png", width = PlotWidth, height = PlotHeight, path = FigDir)

var_box_plot(Annual, "FThaw", cols=colors3, ylab="Cycles/Yr",
             title=paste0("Average annual freeze-thaw cycles in ", Yr, " vs ", BasePeriod))
ggsave("Annual-box-FThaw.png", width = PlotWidth, height = PlotHeight, path = FigDir)

var_line_plot(Annual, var=FThaw, cols=col, title=paste0("Annual freeze-thaw cycles"),
              ylab="Cycles/Yr")
ggsave("Annual-line-FThaw.png", width = PlotWidth, height = PlotHeight, path = FigDir)


## GDD
var_bar_plot(Annual, "GDD", cols=colors3, ylab="Days/Yr",
             title=paste0("Average annual growing degree days (Tavg > 41\u00B0F) in ", Yr, " vs ", BasePeriod))
ggsave("Annual-bar-GDD.png", width = PlotWidth, height = PlotHeight, path = FigDir)

var_box_plot(Annual, "GDD", cols=colors3, ylab="Days/Yr",
             title=paste0("Average annual growing degree days (Tavg > 41\u00B0F) in ", Yr, " vs ", BasePeriod))
ggsave("Annual-box-GDD.png", width = PlotWidth, height = PlotHeight, path = FigDir)

var_line_plot(Annual, var=GDD, cols=col, title=paste0("Annual growing degree days (Tavg > 41\u00B0F)"),
              ylab="Days/Yr")
ggsave("Annual-line-GDD.png", width = PlotWidth, height = PlotHeight, path = FigDir)


## GrowLen
var_bar_plot(Annual, "GrowLen", cols=colors3, ylab="Days/Yr",
             title=paste0("Average annual growing season length in ", Yr, " vs ", BasePeriod))
ggsave("Annual-bar-GrowLen.png", width = PlotWidth, height = PlotHeight, path = FigDir)

var_box_plot(Annual, "GrowLen", cols=colors3, ylab="Days/Yr",
             title=paste0("Average annual growing degree season length in ", Yr, " vs ", BasePeriod))
ggsave("Annual-box-GrowLen.png", width = PlotWidth, height = PlotHeight, path = FigDir)

var_line_plot(Annual, var=GrowLen, cols=col, title=paste0("Annual growing season length"),
              ylab="Days/Yr")
ggsave("Annual-line-GrowLen.png", width = PlotWidth, height = PlotHeight, path = FigDir)


## BegGrow
var_bar_plot(Annual, "BegGrow", cols=colors3, ylab="Julian Day",
             title=paste0("Average annual green-up date (Julian day) in ", Yr, " vs ", BasePeriod))
ggsave("Annual-bar-BegGrow.png", width = PlotWidth, height = PlotHeight, path = FigDir)

var_box_plot(Annual, "BegGrow", cols=colors3, ylab="Julian Day",
             title=paste0("Average annual green-up date (Julian day) in ", Yr, " vs ", BasePeriod))
ggsave("Annual-box-BegGrow.png", width = PlotWidth, height = PlotHeight, path = FigDir)

var_line_plot(Annual, var=BegGrow, cols=col, title=paste0("Annual green-up date (Julian day)"),
              ylab="Julian Day")
ggsave("Annual-line-BegGrow.png", width = PlotWidth, height = PlotHeight, path = FigDir)


## Sp.Frost
var_bar_plot(Annual, "Sp.Frost", cols=colors3, ylab="Days/Yr",
             title=paste0("Average annual spring frost days (Tavg>41 & Tmin<32) in ", Yr, " vs ", BasePeriod))
ggsave("Annual-bar-Sp.Frost.png", width = PlotWidth, height = PlotHeight, path = FigDir)

var_box_plot(Annual, "Sp.Frost", cols=colors3, ylab="Days/Yr",
             title=paste0("Average annual spring frost days (Tavg>41 & Tmin<32) in ", Yr, " vs ", BasePeriod))
ggsave("Annual-box-Sp.Frost.png", width = PlotWidth, height = PlotHeight, path = FigDir)

var_line_plot(Annual, var=Sp.Frost, cols=col, title=paste0("Annual spring frost days (Tavg>41 & Tmin<32)"),
              ylab="Days/Yr")
ggsave("Annual-line-Sp.Frost.png", width = PlotWidth, height = PlotHeight, path = FigDir)


## HI.Dan
var_bar_plot(Annual, "HI.Dan", cols=colors3, ylab="Days/Yr",
             title=paste0("Average annual dangerous heat index days in ", Yr, " vs ", BasePeriod))
ggsave("Annual-bar-HI.Dan.png", width = PlotWidth, height = PlotHeight, path = FigDir)

var_box_plot(Annual, "HI.Dan", cols=colors3, ylab="Days/Yr",
             title=paste0("Average annual dangerous heat index days in ", Yr, " vs ", BasePeriod))
ggsave("Annual-box-HI.Dan.png", width = PlotWidth, height = PlotHeight, path = FigDir)

var_line_plot(Annual, var=HI.Dan, cols=col, title=paste0("Annual dangerous heat index days"),
              ylab="Days/Yr")
ggsave("Annual-line-HI.Dan.png", width = PlotWidth, height = PlotHeight, path = FigDir)


############################################### PRINT TABLES #################################################################
A<-aggregate(.~CF,Annual[,c(1,3:27)], mean) 
S<-aggregate(.~CF,Annual[,c(1,3:27)],sd) #Withing CF
WB_GCM_all <- rbind(H_annual,subset(F_annual, GCM %in% WB_GCMs$GCM))
WB_GCM_Means <-aggregate(.~CF,WB_GCM_all[,c(1,4:24)], mean, na.rm=TRUE)
WB_GCM_SD <-aggregate(.~CF,WB_GCM_all[,c(1,4:24)], sd, na.rm=TRUE)

WB_GCM_YOY <- split(WB_GCM_all,WB_GCM_all$CF)
for (i in 1:length(WB_GCM_YOY)){
  # name=names(WB_GCM_YOY)[i]
  WB_GCM_YOY[[i]][,4:24] <- 
    lag(WB_GCM_YOY[[i]][,4:24] ,1) - WB_GCM_YOY[[i]][,4:24] 
}

WB_GCM_YOY <- ldply(WB_GCM_YOY, data.frame)
YOY <- aggregate(.~CF,WB_GCM_YOY[,c(2,5:length(WB_GCM_YOY))], FUN=mean, na.rm=TRUE)


write.xlsx(list("Means"=A,"Annual"=Annual,"Season"=Season,"D_Season"=Season_delta,"Monthly"=Monthly,"Monthly_delta"=Monthly_delta,
                "SD"=S, "WB_GCM_Means"=WB_GCM_Means, "WB_GCM_SD"=WB_GCM_SD,"WB_GCM_YOY"=YOY), 
           file=(paste0(TableDir,"Plot_data.xlsx")),col.names=TRUE)
rm(S,A,WB_GCM_Means,WB_GCM_SD,WB_GCM_YOY,YOY)
