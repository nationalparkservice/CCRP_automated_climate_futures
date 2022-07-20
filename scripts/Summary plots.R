###################################################################################################################

Future_summary <- merge(ALL_FUTURE,CF_GCM,by="GCM")
Baseline_summary <- Gridmet; Baseline_summary$CF = "Historical"
all_summary <- rbind(Baseline_summary, Future_summary)
all_summary <- subset(all_summary, GCM %in% WB_GCMs$GCM | GCM == "gridmet.historical")
all_summary$CF <- factor(all_summary$CF, levels=c("Historical", CFs))

AnnualWB <- read.csv(paste0(TableDir,"WB-Annual.csv")) %>% 
  mutate(max_pack.in = max_pack.mm/ 25.4,
         runoff.in = runoff.mm/ 25.4,
         sum_d.in = sum_d.mm/ 25.4,
         sum_aet.in = sum_aet.mm/ 25.4) %>% rename(Year=year) %>% left_join(WB_GCMs,by="GCM")


########################################### Format MACA data #######################################################
yrAvgs <- aggregate(cbind(TavgF, PrcpIn)~Year+CF,all_summary,mean)
yrAvgs$PrcpIn <- yrAvgs$PrcpIn * 365
yrAvgs$TavgRoll10 <- rollmean(yrAvgs$TavgF, rollLen, fill = NA, align = "right")
yrAvgs$PrcpRoll10 <- rollmean(yrAvgs$Prcp, rollLen, fill = NA, align = "right")

WBAvgs <- aggregate(cbind(sum_d.in, runoff.in, max_pack.in)~Year+CF, AnnualWB, sum)
WBAvgs$D.inRoll10 <- rollmean(WBAvgs$sum_d.in, rollLen, fill = NA, align = "right")
WBAvgs$Runoff.inRoll10 <- rollmean(WBAvgs$runoff.in, rollLen, fill = NA, align = "right")
WBAvgs$SWEaccum.inRoll10 <- rollmean(WBAvgs$max_pack.in, rollLen, fill = NA, align = "right")
WBAvgs$Year <- as.numeric(WBAvgs$Year)


# Tmean
t<-LT_plot(yrAvgs,TavgF,rollvar=TavgRoll10,cols=col,yaxis="Mean annual temperature (\u00B0F)",title="") 
ggsave("TavgF-Timeseries.png",t, path = FigDir, height=PlotHeight, width=PlotWidth)

# Precip
p<-LT_plot(yrAvgs,PrcpIn,rollvar=PrcpRoll10,cols=col,yaxis="Mean annual precipitation (inches/Yr)",title="")
ggsave("PrcpIn-Timeseries.png", p, path = FigDir, height=PlotHeight, width=PlotWidth)

# Deficit
d<-LT_plot(WBAvgs,sum_d.in,rollvar=D.inRoll10,cols=col,yaxis="Mean annual climatic water deficit (in/year)",title="")
ggsave("D.in-Timeseries.png", d, path = FigDir, height=PlotHeight, width=PlotWidth)

# Runoff
r<-LT_plot(WBAvgs, runoff.in, rollvar=Runoff.inRoll10, cols=col,yaxis="Mean annual runoff (in/year)",title="")
ggsave("Runoff.in-Timeseries.png", r, path = FigDir, height=PlotHeight, width=PlotWidth)

# SWEaccum
s<-LT_plot(WBAvgs, max_pack.in, rollvar=SWEaccum.inRoll10,cols=col,yaxis="Mean annual accumulated SWE (in/year)",title="")
ggsave("SWEaccum.in-Timeseries.png", path = FigDir, height=PlotHeight, width=PlotWidth)


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