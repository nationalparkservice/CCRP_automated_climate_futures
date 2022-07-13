
###################################################################################################################

Future_summary <- merge(ALL_FUTURE,CF_GCM,by="GCM")
Baseline_summary <- Gridmet; Baseline_summary$CF = "Historical"
all_summary <- rbind(Baseline_summary, Future_summary)
all_summary <- subset(all_summary, GCM %in% WB_GCMs$GCM | GCM == "gridmet.historical")
all_summary$CF <- factor(all_summary$CF, levels=c("Historical", CFs))

WB_summary <- subset(WBdat, GCM %in% WB_GCMs$GCM | GCM == "gridmet.historical")
WB_summary$CF <- factor(WB_summary$CF, levels=c("Historical", CFs))


########################################### Format MACA data #######################################################
yrAvgs <- aggregate(cbind(TavgF, PrcpIn)~Year+CF,all_summary,mean)
yrAvgs$PrcpIn <- yrAvgs$PrcpIn * 365
yrAvgs$TavgRoll10 <- rollmean(yrAvgs$TavgF, rollLen, fill = NA, align = "right")
yrAvgs$PrcpRoll10 <- rollmean(yrAvgs$Prcp, rollLen, fill = NA, align = "right")

WBAvgs <- aggregate(cbind(D.in, Runoff.in, SWEaccum.in)~Year+CF, WB_summary, sum)
WBAvgs$D.inRoll10 <- rollmean(WBAvgs$D.in, rollLen, fill = NA, align = "right")
WBAvgs$Runoff.inRoll10 <- rollmean(WBAvgs$Runoff.in, rollLen, fill = NA, align = "right")
WBAvgs$SWEaccum.inRoll10 <- rollmean(WBAvgs$SWEaccum.in, rollLen, fill = NA, align = "right")


# Tmean
t<-LT_plot(yrAvgs,TavgF,rollvar=TavgRoll10,cols=col,yaxis="Mean annual temperature (\u00B0F)",title="") 
ggsave("LT-TavgF.png",t, path = FigDir, height=PlotHeight, width=PlotWidth)

# Precip
p<-LT_plot(yrAvgs,PrcpIn,rollvar=PrcpRoll10,cols=col,yaxis="Mean annual precipitation (inches/Yr)",title="")
ggsave("LT-PrcpIn.png", p, path = FigDir, height=PlotHeight, width=PlotWidth)

# Deficit
d<-LT_plot(WBAvgs,D.in,rollvar=D.inRoll10,cols=col,yaxis="Mean annual climatic water deficit (in/year)",title="")
ggsave("LT-D.in.png", d, path = FigDir, height=PlotHeight, width=PlotWidth)

# Runoff
r<-LT_plot(WBAvgs, Runoff.in, rollvar=Runoff.inRoll10, cols=col,yaxis="Mean annual runoff (in/year)",title="")
ggsave("LT-Runoff.in.png", r, path = FigDir, height=PlotHeight, width=PlotWidth)

# SWEaccum
s<-LT_plot(WBAvgs, SWEaccum.in, rollvar=SWEaccum.inRoll10,cols=col,yaxis="Mean annual accumulated SWE (in/year)",title="")
ggsave("LT-SWEaccum.in.png", path = FigDir, height=PlotHeight, width=PlotWidth)


###### PANELS ##########

#Temp and Precip
legend <- grid_arrange_shared_legend(t,p,nrow=2,ncol=1,position="bottom")
g <- grid.arrange(legend,top = textGrob(paste0("Historical and future projections for ", SiteID),
                                        gp=gpar(fontface="bold", col="black", fontsize=26)))
ggsave("LT-TavgF-PrcpIn.png", g, path = FigDir, height=PanelHeight, width=PanelWidth)

#Temp and Deficit
legend <- grid_arrange_shared_legend(t,d,nrow=2,ncol=1,position="bottom")
g <- grid.arrange(legend,top = textGrob(paste0("Historical and future projections for ", SiteID),
                                        gp=gpar(fontface="bold", col="black", fontsize=26)))
ggsave("LT-TavgF_D.in.png", g, path = FigDir, height=PanelHeight, width=PanelWidth)

#Temp and Runoff
legend <- grid_arrange_shared_legend(t,r,nrow=2,ncol=1,position="bottom")
g <- grid.arrange(legend,top = textGrob(paste0("Historical and future projections for ", SiteID),
                                        gp=gpar(fontface="bold", col="black", fontsize=26)))
ggsave("LT-TavgF_Runoff.in.png", g, path = FigDir, height=PanelHeight, width=PanelWidth)

#Temp and SWE
legend <- grid_arrange_shared_legend(t,s,nrow=2,ncol=1,position="bottom")
g <- grid.arrange(legend,top = textGrob(paste0("Historical and future projections for ", SiteID),
                                        gp=gpar(fontface="bold", col="black", fontsize=26)))
ggsave("LT-TavgF_SWEaccum.in.png", g, path = FigDir, height=PanelHeight, width=PanelWidth)

##################
