# Delta tmax and tmin
a <- dot_plot(Monthly_delta, TminF, Month, grp=CF, cols=colors2,
         title = paste(""),
         xlab="Change in minimum temperature (\u00B0F)",
         ylab="Months",
         labels=MonthLabels)
b <- dot_plot(Monthly_delta, TmaxF, Month, grp=CF, cols=colors2,
              title = paste(""),
              xlab="Change in maximum temperature (\u00B0F)",
              ylab=" ",
              labels=MonthLabels)

legend <- grid_arrange_shared_legend(a,b+ rremove("y.text"),
                                     ncol=2,nrow=1,position="bottom", 
                                     top=textGrob(paste0("Historical and future projections for ", SiteID),
                                              gp=gpar(fontface="bold", col="black", fontsize=26)))
annotate_figure(legend,fig.lab=if(MethodCaption == "Y"){"Q"},fig.lab.pos = "bottom.right")
ggsave("TminF-TmaxF-Panel.png", path = FigDir, height=PanelHeight, width=PanelWidth,bg="white")


# Extreme heat: heat index + Tmax99
a <- var_bar_plot(Annual, "Tmax99", cols=colors3, ylab="Days/Yr",
             title=paste0(SiteID, "-Average Days/Yr > Historical 99th percentile (", round(HistTmax99,1), " \u00B0F )"))
b<- var_bar_plot(Annual, "HI.Dan", cols=colors3, ylab="Days/Yr",
             title=paste0("Average annual dangerous heat index days"))
g <- grid.arrange(a,b,nrow=2)
figure <- ggarrange(a + rremove("ylab") + rremove("x.text"), b + rremove("ylab"), # remove axis labels from plots
                    labels = NULL,
                    nrow = 2)

annotate_figure(figure, left = textGrob("Days/Yr", rot = 90, vjust = 1, gp = gpar(cex = 2)),
                fig.lab=if(MethodCaption == "Y"){"Q"},fig.lab.pos = "bottom.right")
ggsave("OverTmax99-HI.Dan-Panel.jpg", path = FigDir, height=PanelHeight, width=PanelWidth,bg="white")


# Extreme precip: return intervals + LT-runoff
a <- ggplot(allregressions, aes(x=return, y=GEV, group=CF, colour = CF)) +
  geom_line(size = 2, stat = "identity",colour="black") + 
  geom_line(size = 1.5, stat = "identity") +
  geom_point(colour= "black", size=4, aes(fill = factor(CF), shape = factor(CF))) +
  PlotTheme + theme(axis.title.x=element_text(size=24, vjust=0.5,  margin=margin(t=20, r=20, b=20, l=20))) +
  labs(title = paste(SiteID, " - Recurrence intervals for \n24-hour precipitation totals",sep=""),
       x = "Recurrence interval (year)", y = "Precipitation (inches/day)") +
  scale_color_manual(name="",values = colors3) +
  scale_fill_manual(name="",values = colors3) +
  scale_shape_manual(name="",values = c(21,22,23))
  
b <- LT_plot(WBAvgs, runoff.in, rollvar=Runoff.inRoll10, cols=col,yaxis="Runoff (in/year)",title="Mean annual runoff ")
legend <- grid_arrange_shared_legend(a,b,nrow=2,ncol=1,position="bottom")
annotate_figure(legend,fig.lab=if(MethodCaption == "Y"){"I"},fig.lab.pos = "bottom.right")
ggsave("Recurrenceinterval-Runoff.in-Panel.png", path = FigDir, height=PanelHeight, width=PanelWidth,bg="white")


# Extreme precip: return intervals + OverPrecip99
a <- ggplot(allregressions, aes(x=return, y=GEV, group=CF, colour = CF)) +
  geom_line(size = 2, stat = "identity",colour="black") + 
  geom_line(size = 1.5, stat = "identity") +
  geom_point(colour= "black", size=4, aes(fill = factor(CF), shape = factor(CF))) +
  PlotTheme + theme(axis.title.x=element_text(size=24, vjust=0.5,  margin=margin(t=20, r=20, b=20, l=20))) +
  labs(title = paste(SiteID, " - Recurrence intervals for \n24-hour precipitation totals",sep=""),
       x = "Recurrence interval (year)", y = "Precipitation (inches/day)",caption=
         if(MethodCaption == "Y"){"I"}) +
  scale_color_manual(name="",values = colors3) +
  scale_fill_manual(name="",values = colors3) +
  scale_shape_manual(name="",values = c(21,22,23))

b <- var_bar_plot(Annual, "OverPrecip99", cols=colors3, ylab="Days/Yr",
             title=paste0("Average Days/Yr Precipitation > Historical 99th\n Percentile (", round(HistPr99, 1), " in) in ", Yr, " vs ", BasePeriod))
g <- grid.arrange(a,b,nrow=2)
annotate_figure(g, fig.lab=if(MethodCaption == "Y"){"Q"},fig.lab.pos = "bottom.right")
ggsave("Recurrenceinterval-OverPrecip99-Panel.png", path = FigDir, height=PanelHeight, width=PanelWidth,bg="white")



# Fire: WaterBalance, AET
a<-LT_plot(WBAvgs,sum_d.in,rollvar=D.inRoll10,cols=col,yaxis="Mean annual climatic water\n deficit (in/year)",
           title=paste("Water Balance for ",SiteID,sep=""))
b <- ggplot(AnnualWB %>% filter(Year <2013 | (Year >=Yr - (Range/2) & Year <= (Yr + (Range/2)))),
                   aes(x=sum_d.in, y=sum_aet.in, colour=CF)) + geom_point(size=3)+ geom_smooth(method="lm", se=FALSE, size=2)+
  scale_colour_manual("",values=col) +
  labs(
    y = "Annual Actual Evapotranspiration (in)",
    x = "Annual water deficit (in)",
    colour = "GCM",
    title = ""  
  ) + PlotTheme + theme(axis.title.x=element_text(size=24, vjust=0.5,  margin=margin(t=20, r=20, b=20, l=20)))
legend <- grid_arrange_shared_legend(a,b,nrow=2,ncol=1,position="bottom")
annotate_figure(legend,fig.lab=if(MethodCaption == "Y"){"I"},fig.lab.pos = "bottom.right")
ggsave("Panel-D.in-WaterBalance.png", path = FigDir, height=PanelHeight, width=PanelWidth,bg="white")


# Fire: WaterBalance, VPD
a<-LT_plot(WBAvgs,sum_d.in,rollvar=D.inRoll10,cols=col,yaxis="Mean annual climatic water\n deficit (in/year)",
           title=paste("Water Balance for ",SiteID,sep=""))
b <- Month_bar_plot(Monthly_delta,xvar=Month,yvar=VPD,grp=CF,cols=colors2,
                 title=paste0("Change in avg monthly VPD in ",Yr," vs ", BasePeriod),
                 xlab = "Month", ylab="Change in VPD (kPa)",label=MonthLabels)
legend <- grid_arrange_shared_legend(a,b,nrow=2,ncol=1,position="bottom")
annotate_figure(legend,fig.lab=if(MethodCaption == "Y"){"I"},fig.lab.pos = "bottom.right")
ggsave("Panel-D.in-VPD.png",path = FigDir, height=PanelHeight, width=PanelWidth,bg="white")


#Drought: Multi-panel ts and characteristic bar plots
## LOCATED IN RSS_MACA_drought_char.R