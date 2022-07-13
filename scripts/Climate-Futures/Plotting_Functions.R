##Plot parameters

#X-axis labels for monthly plots
MonthLabels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")
SeasonLabels = c("Winter", "Spring","Summer", "Fall")

degF <- "(\u00B0F)"

#Height and width 
PlotWidth = 15
PlotHeight = 9

#Panel plot width and height
PanelWidth = 15
PanelHeight = 15

#ggplot theme to control formatting parameters for plots with month on the x-axis
PlotTheme = theme(axis.text=element_text(size=20),    #Text size for axis tick mark labels
                  axis.title.x=element_blank(),               #Text size and alignment for x-axis label
                  axis.title.y=element_text(size=24, vjust=0.5,  margin=margin(t=20, r=20, b=20, l=20)),              #Text size and alignment for y-axis label
                  plot.title=element_text(size=26,face="bold",hjust=0.5, margin=margin(t=20, r=20, b=20, l=20)),      #Text size and alignment for plot title
                  legend.title=element_text(size=24),                                                                    #Text size of legend category labels
                  legend.text=element_text(size=22),                                                                   #Text size of legend title
                  legend.position = "bottom")  

BarPlotTheme = theme(axis.text.x=element_text(size=24),    #Text size for axis tick mark labels
                     axis.text.y=element_text(size=20),
                     axis.title.x=element_blank(),               #Text size and alignment for x-axis label
                     axis.title.y=element_text(size=24, vjust=0.5,  margin=margin(t=20, r=20, b=20, l=20)),              #Text size and alignment for y-axis label
                     plot.title=element_text(size=26,face="bold",hjust=0.5, margin=margin(t=20, r=20, b=20, l=20)),      #Text size and alignment for plot title
                     legend.position = "none") 

############ PLOT FUNCTIONS #############
Month_line_plot <- function(data, xvar, yvar, grp, cols, title,xlab, ylab){
ggplot(data, aes(x={{xvar}}, y={{yvar}}, group={{grp}}, colour = {{grp}})) +
  geom_line(size = 2, stat = "identity",colour="black") + 
  geom_line(size = 1.5, stat = "identity") +
  geom_point(colour= "black", size=4, aes(fill = factor({{grp}}), shape = factor({{grp}}))) +
  PlotTheme +
  labs(title = title,
       x = xlab, y = ylab) +
  scale_color_manual(name="",values = cols) +
  scale_fill_manual(name="",values = cols) +
  scale_shape_manual(name="",values = c(seq(21,21+length(cols)-1,1))) +
  # scale_y_continuous(limits=c(0, ceiling(max(eval(parse(text=paste0(data,"$",yvar))))))) +
  scale_x_discrete(labels = MonthLabels)
}


dot_plot <- function(data, xvar, yvar, grp, cols, title,xlab,ylab,labels){
  ggplot(data, aes(x={{xvar}},y={{yvar}},fill={{grp}})) +
    geom_vline(xintercept=0, linetype="dashed", color = "black") + 
    geom_point(stat="identity",size=8,colour="black",aes(fill = factor({{grp}}), shape = factor({{grp}}))) +
    PlotTheme +
    theme(axis.title.x=element_text(size=20, vjust=0.5)) +
    labs(title = title, 
         x = xlab, y = ylab) +
    scale_fill_manual(name="",values =cols) +
    scale_shape_manual(name="",values = c(seq(21,21+length(cols)-1,1))) +
    scale_y_discrete(labels=rev(labels), limits=rev)
}


Month_bar_plot <- function(data, xvar, yvar, grp, cols, title,xlab, ylab,labels){
  ggplot(data, aes(x={{xvar}},y={{yvar}},fill={{grp}})) +
    geom_bar(stat="identity",position="dodge",colour="black") +
    PlotTheme +
    labs(title = title, 
         x = xlab, y = ylab) +
    scale_fill_manual(name="",values = cols) +
    scale_x_discrete(labels = labels)
}


var_bar_plot <- function(data,var, cols, title, ylab){
At<-aggregate(eval(parse(text=var))~CF,data=data,mean);
names(At)<-c("CF",var)
ggplot(At, aes(x=CF,y=(eval(parse(text=var))),fill=CF)) +
  geom_bar(stat="identity",position="dodge",colour="black") +
  BarPlotTheme +
  # coord_cartesian(ylim=c(0, 40)) +
  labs(title = title, 
       y = ylab, colour = "Climate Future")  +
  scale_fill_manual(name="",values = cols) +
  coord_cartesian(ylim = c(min(eval(parse(text=paste("At$",var,sep="")))), max(eval(parse(text=paste("At$",var,sep=""))))))
}


var_box_plot <- function(data,var, cols, title, ylab){
  p<-ggplot(data, aes(x=CF, y=(eval(parse(text=var))), colour=CF)) + 
    geom_boxplot(colour="black",aes(fill = factor(CF)), outlier.shape=NA)+ 
    geom_jitter(shape = 21, size = 5, aes(fill = factor(CF),colour=factor(me.col)), position=position_jitter(0.2)) +
    BarPlotTheme +
    labs(title = title, 
         y = ylab) +
    scale_color_manual(name="",values = c("black","white"),guide=FALSE) +
    scale_fill_manual(name="",values = cols)
  dat<-ggplot_build(p)$data[[1]];dat1<-dat[3,]
  p + geom_segment(data=dat1, aes(x=xmin, xend=xmax, 
                                  y=middle, yend=middle), colour="grey", size=1)
}


var_line_plot <- function(data,var, cols, title, ylab) {
ggplot(data, aes(x=as.numeric(Year), y={{var}}, col=CF, fill=CF)) + 
  geom_line(size=2) + geom_point(col="black", size=2, shape=16) +
  geom_point() +
  labs(title = title, x="Year", y=ylab) +
  scale_color_manual(name="Climate Future",values=cols) +
  scale_fill_manual(name="Climate Future",values=cols) + PlotTheme 
}


density_plot <- function(data, xvar, cols,title, xlab) {
  ggplot(data, aes(x={{xvar}}, colour=CF,fill=CF,linetype=CF),show.legend=F) +geom_density(alpha=0.3,size=1.5) +
    scale_colour_manual(name="",values=cols) +
    scale_fill_manual(name="",values=cols) +  
    scale_linetype_manual(name="",values=seq(1,1+length(CFs),1)) +
    labs(y = "Density",
         x = xlab,
         title = title) +
    PlotTheme
}

spaghetti_plot_wateryr <- function(data, var, col,CF){
  df <- deparse(substitute(data))
    ggplot() +
      geom_line(data=data,aes(x=WaterYr,y=eval(parse(text=var)),group=Year),colour=col,size=1) +
      geom_vline(xintercept=183, linetype="dashed", color = "black") +
      geom_text(aes(x=183, label="Apr 1\n", y=max(eval(parse(text=paste0("WBData", "$",var))))), 
                colour="black", angle=90, text=element_text(size=11),hjust=1) +
      geom_vline(xintercept=1, linetype="dashed", color = "black") +
      geom_text(aes(x=1, label="\nOct 1", y=max(eval(parse(text=paste0("WBData", "$",var))))),
                colour="black", angle=90, text=element_text(size=11),hjust=1) +
      coord_cartesian(ylim = c(0, max(eval(parse(text=paste0("WBData","$",var))))))+
      # scale_x_date(breaks = date_breaks("months"),labels = date_format("%b")) +
      PlotTheme + 
      theme(plot.title=element_text(size=16,hjust=0.5,face="plain", margin=margin(t=1, r=1, b=1, l=1))) +
      labs(title = CF, 
           x = "", y = "")
}


LT_plot <-function(data,yvar, rollvar,cols,yaxis,title){
  ggplot(data, aes(x=Year, y={{yvar}}, col=CF, fill=CF)) + 
    geom_rect(xmin=Yr-Range/2, xmax=Yr+Range/2, ymin=0, ymax=80, alpha=0.1, fill="lightgray", col="lightgray") +
    # geom_ribbon(aes(x=as.numeric(as.character(year)), ymin=Tavg.min, ymax=Tavg.max, fill=CF), alpha=0.5) +
    geom_line(size=2) + geom_point(col="black", size=2, shape=16) +
    geom_point() +
    geom_line(aes(x=Year, y={{rollvar}}),size=1.25,colour="black", na.rm=TRUE) +
    geom_line(aes(x=Year, y={{rollvar}},colour = CF), size=.75 ,na.rm=TRUE) +
    scale_x_continuous(breaks=c(1980, 2000, 2020, 2040, 2060, 2080, 2100)) +
    labs(x="Year", y=yaxis,title=title) +
    scale_color_manual(name="Climate Future",values=cols) +
    scale_fill_manual(name="Climate Future",values=cols) + PlotTheme +
    theme( axis.line = element_line(colour = "black"), #Add axis lines
           panel.background = element_blank(), #Background white
           panel.grid.major = element_line("light grey",0.3)) #add grid back)
}


