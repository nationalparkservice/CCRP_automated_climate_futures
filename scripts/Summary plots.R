###################################################################################################################

Future_summary <- merge(ALL_FUTURE,CF_GCM,by="GCM")
Baseline_summary <- Gridmet; Baseline_summary$CF = "Historical";Baseline_summary$RCP = "Historical"
all_summary <- rbind(Baseline_summary, Future_summary)
all_summary <- subset(all_summary, GCM %in% WB_GCMs$GCM | GCM == "gridmet.historical")
all_summary$CF <- factor(all_summary$CF, levels=c("Historical", CFs))

AnnualWB <- read.csv(paste0(TableDir,"WB-Annual.csv")) %>% 
  mutate(max_pack.in = max_pack.mm/ 25.4,
         runoff.in = runoff.mm/ 25.4,
         sum_d.in = sum_d.mm/ 25.4,
         sum_aet.in = sum_aet.mm/ 25.4) %>% rename(Year=year) %>% left_join(WB_GCMs,by="GCM") %>% 
  mutate(CF = ifelse(is.na(CF), "Historical", CF),
         CF = factor(CF, levels=c("Historical",CFs))) 


########################################### Format MACA data #######################################################
yrAvgs <- aggregate(cbind(TavgF, PrcpIn)~Year+CF,all_summary,mean)
yrAvgs$PrcpIn <- yrAvgs$PrcpIn * 365
yrAvgs <- yrAvgs %>% group_by(CF) %>% 
  mutate(TavgRoll10 = rollmean(TavgF, rollLen, fill=NA, align="right"),
         PrcpRoll10 = rollmean(PrcpIn, rollLen, fill=NA, align="right"),
         Year = as.numeric(Year))

WBAvgs <- aggregate(cbind(sum_d.in, runoff.in, max_pack.in)~Year+CF, AnnualWB, sum) %>% 
  group_by(CF) %>% 
  mutate(D.inRoll10 = rollmean(sum_d.in, rollLen, fill=NA, align="right"),
         Runoff.inRoll10 = rollmean(runoff.in, rollLen, fill=NA, align="right"),
         SWEaccum.inRoll10 = rollmean(max_pack.in, rollLen, fill=NA, align="right"),
         Year = as.numeric(Year))

# Tmean
t<-LT_plot(yrAvgs,TavgF,rollvar=TavgRoll10,cols=col,yaxis="Mean annual temperature (\u00B0F)",title="",CFmethod="I")
ggsave("TavgF-Timeseries.png",t+labs(title=paste0(SiteID,"-Historical and future\n mean annual temperature (\u00B0F)")), path = FigDir, height=PlotHeight, width=PlotWidth)

# Precip
p<-LT_plot(yrAvgs,PrcpIn,rollvar=PrcpRoll10,cols=col,yaxis="Mean annual precipitation (inches/Yr)",title="",CFmethod="I")
ggsave("PrcpIn-Timeseries.png", p+labs(title=paste0(SiteID,"-Historical and future\n mean annual precipitation (inches/Yr)")), path = FigDir, height=PlotHeight, width=PlotWidth)

# Deficit

col2 <- c("darkgray",rev(colors2)) 

d<-LT_plot(WBAvgs,sum_d.in,rollvar=D.inRoll10,cols=col,yaxis="Mean annual climatic \nwater deficit (in/year)",title="",CFmethod="I")
ggsave("D.in-Timeseries.png", d+labs(title=paste0(SiteID,"-Historical and future\n mean annual climatic water deficit (inches/Yr)")), path = FigDir, height=PlotHeight, width=PlotWidth)

# Runoff
r<-LT_plot(WBAvgs, runoff.in, rollvar=Runoff.inRoll10, cols=col,yaxis="Mean annual runoff (in/year)",title="",CFmethod="I")
ggsave("Runoff.in-Timeseries.png", r+labs(title=paste0(SiteID,"-Historical and future\n mean annual runoff (inches/Yr)")), path = FigDir, height=PlotHeight, width=PlotWidth)

# SWEaccum
s<-LT_plot(WBAvgs, max_pack.in, rollvar=SWEaccum.inRoll10,cols=col,yaxis="Mean annual accumulated SWE\n (in/year)",title="",CFmethod="I")
ggsave("SWEaccum.in-Timeseries.png", s+labs(title=paste0(SiteID,"-Historical and future\n mean annual snow water equivalenat (SWE;inches/Yr)")),path = FigDir, height=PlotHeight, width=PlotWidth)


###### PANELS ##########

#Temp and Precip
legend <- grid_arrange_shared_legend(t,p,nrow=2,ncol=1,position="bottom")
g <- grid.arrange(legend,top = textGrob(paste0("Historical and future projections for ", SiteID),
                                        gp=gpar(fontface="bold", col="black", fontsize=26)))
ggsave("TavgF-PrcpIn-Timeseries.png", g, path = FigDir, height=PanelHeight, width=PanelWidth)

#Temp and Deficit
legend <- grid_arrange_shared_legend(t,d,nrow=2,ncol=1,position="bottom")
g <- grid.arrange(legend,top = textGrob(paste0("Historical and future projections for ", SiteID),
                                        gp=gpar(fontface="bold", col="black", fontsize=26)))
ggsave("TavgF_D.in-Timeseries.png", g, path = FigDir, height=PanelHeight, width=PanelWidth)

#Temp and Runoff
legend <- grid_arrange_shared_legend(t,r,nrow=2,ncol=1,position="bottom")
g <- grid.arrange(legend,top = textGrob(paste0("Historical and future projections for ", SiteID),
                                        gp=gpar(fontface="bold", col="black", fontsize=26)))
ggsave("TavgF_Runoff.in-Timeseries.png", g, path = FigDir, height=PanelHeight, width=PanelWidth)

#Temp and SWE
legend <- grid_arrange_shared_legend(t,s,nrow=2,ncol=1,position="bottom")
g <- grid.arrange(legend,top = textGrob(paste0("Historical and future projections for ", SiteID),
                                        gp=gpar(fontface="bold", col="black", fontsize=26)))
ggsave("TavgF_SWEaccum.in-Timeseries.png", g, path = FigDir, height=PanelHeight, width=PanelWidth)

##################