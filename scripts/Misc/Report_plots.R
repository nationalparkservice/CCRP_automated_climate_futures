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
ggsave("Panel-TminF-TmaxF.png",legend, path = FigDir, height=PanelHeight, width=PanelWidth)


# Extreme heat: heat index + Tmax95
a <- var_bar_plot(Annual, "OverHotTemp", cols=colors3, ylab="Days/Yr",
             title=paste0("Average Days/Yr > ", HotTemp, " (\u00B0F)"))
b<- var_bar_plot(Annual, "HI.Dan", cols=colors3, ylab="Days/Yr",
             title=paste0("Average annual dangerous heat index days"))
g <- grid.arrange(a,b,nrow=2)
figure <- ggarrange(a + rremove("ylab") + rremove("x.text"), b + rremove("ylab"), # remove axis labels from plots
                    labels = NULL,
                    nrow = 2)

annotate_figure(figure, left = textGrob("Days/Yr", rot = 90, vjust = 1, gp = gpar(cex = 2)))
ggsave("Panel-OverHotTemp-HI.Dan.png", path = FigDir, height=PanelHeight, width=PanelWidth)


# Extreme precip: return intervals + LT-runoff
a <- ggplot(allregressions, aes(x=return, y=modeled, group=CF, colour = CF)) +
  geom_line(size = 2, stat = "identity",colour="black") + 
  geom_line(size = 1.5, stat = "identity") +
  geom_point(colour= "black", size=4, aes(fill = factor(CF), shape = factor(CF))) +
  PlotTheme + theme(axis.title.x=element_text(size=24, vjust=0.5,  margin=margin(t=20, r=20, b=20, l=20))) +
  labs(title = paste(SiteID, " - Recurrence intervals for 24-hour precipitation totals",sep=""),
       x = "Recurrence interval (year)", y = "Precipitation (inches/day)") +
  scale_color_manual(name="",values = colors3) +
  scale_fill_manual(name="",values = colors3) +
  scale_shape_manual(name="",values = c(21,22,23))
  
b <- LT_plot(WBAvgs, Runoff.in, rollvar=Runoff.inRoll10, cols=col,yaxis="Runoff (in/year)",title="Mean annual runoff ")
legend <- grid_arrange_shared_legend(a,b,nrow=2,ncol=1,position="bottom")
ggsave("Panel-recurrenceinterval-Runoff.in.png",legend, path = FigDir, height=PanelHeight, width=PanelWidth)


# Extreme precip: return intervals + OverPrecip95
a <- ggplot(allregressions, aes(x=return, y=modeled, group=CF, colour = CF)) +
  geom_line(size = 2, stat = "identity",colour="black") + 
  geom_line(size = 1.5, stat = "identity") +
  geom_point(colour= "black", size=4, aes(fill = factor(CF), shape = factor(CF))) +
  PlotTheme + theme(axis.title.x=element_text(size=24, vjust=0.5,  margin=margin(t=20, r=20, b=20, l=20))) +
  labs(title = paste(SiteID, " - Recurrence intervals for 24-hour precipitation totals",sep=""),
       x = "Recurrence interval (year)", y = "Precipitation (inches/day)") +
  scale_color_manual(name="",values = colors3) +
  scale_fill_manual(name="",values = colors3) +
  scale_shape_manual(name="",values = c(21,22,23))

b <- var_bar_plot(Annual, "OverPrecip95", cols=colors3, ylab="Days/Yr",
             title=paste0("Average Days/Yr Precipitation > Historical 95th Percentile (", round(HistPrecip95, 1), " in)\n in ", Yr, " vs ", BasePeriod))
g <- grid.arrange(a,b,nrow=2)
ggsave("Panel-recurrenceinterval-OverPrecip95.png", plot=g,path = FigDir, height=PanelHeight, width=PanelWidth)



# Fire: WaterBalance, AET
a<-LT_plot(WBAvgs,D.in,rollvar=D.inRoll10,cols=col,yaxis="Mean annual climatic water deficit (in/year)",
           title=paste("Water Balance for ",SiteID,sep=""))
b <- ggplot(AnnualWB, aes(x=sum_d.in, y=sum_aet.in, colour=CF)) + geom_point(size=3)+ geom_smooth(method="lm", se=FALSE, size=2)+
  scale_colour_manual("",values=col) +
  labs(
    y = "Annual Actual Evapotranspiration (in)",
    x = "Annual water deficit (in)",
    colour = "GCM",
    title = ""  
  ) + PlotTheme + theme(axis.title.x=element_text(size=24, vjust=0.5,  margin=margin(t=20, r=20, b=20, l=20)))
legend <- grid_arrange_shared_legend(a,b,nrow=2,ncol=1,position="bottom")
ggsave("Panel-D.in-WaterBalance.png",legend, path = FigDir, height=PanelHeight, width=PanelWidth)


#Drought: Multi-panel ts and characteristic bar plots
## LOCATED IN RSS_MACA_drought_char.R