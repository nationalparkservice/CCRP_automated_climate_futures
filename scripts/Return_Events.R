##### Testing return period / exceedance probability calculations
exceedance <- function(df, var) { #var name must be in paren
    DF<-df
    DF<-DF[order(-DF[,var]),]
    DF$rank<-seq(1:nrow(DF))
    DF$return<- (nrow(DF)+1)/(DF$rank)
    DF$EP <- 1/DF$return
    DF
  }

############### Analysis on MACA data###########################
# Historical data
# Annual max and plot for cF1
Base_max<-aggregate(PrcpIn~Year,Baseline_all,max)
Base_exceedance <-exceedance(Base_max, "PrcpIn")

regression<-lm(PrcpIn~log(return),data=Base_exceedance)
Base_exceedance$modeled<-predict(regression)

max100base<-data.frame(return=seq(1,100,1))
max100base$modeled<-predict(regression,newdata=max100base)
max100base$GCM<-"Historical"

return50base<-data.frame(return=50)
return50base$modeled<-predict(regression,newdata=return50base)
return50base$GCM<-"Historical"

### CFs future 
Future_subset <- subset(Future_all, GCM %in% WB_GCMs$GCM & CF %in% CFs)
Future_split <- aggregate(PrcpIn~Year+GCM,Future_subset,max)

Future_GCM<-split(Future_split,Future_split$GCM)

future_exceedance <- list()
for (i in 1:length(Future_GCM)){
fe <- exceedance(Future_GCM[[i]],"PrcpIn")
fe$GCM = Future_GCM[[i]]$GCM
future_exceedance[[i]] <- fe
}

future_exceedance<-ldply(future_exceedance,data.frame)


#modeled results
Future_GCM <- split(future_exceedance,future_exceedance$GCM)
max100future <- data.frame()
return50future <- data.frame()

for (i in 1:length(Future_GCM)){
  gcm = unique(Future_GCM[[i]]$GCM)
  regression = lm(PrcpIn~log(return),data=Future_GCM[[i]])
  Future_GCM[[i]]$modeled = predict(regression)
  mf <- data.frame(return=seq(1,100,1))
  mf$modeled<-predict(regression,newdata=mf)
  mf$GCM <- gcm
  max100future <- rbind(max100future,mf)
  
  rf<-data.frame(return=50)
  rf$modeled<-predict(regression,newdata=rf)
  rf$GCM <- gcm
  return50future <- rbind(return50future, rf)
  rm(mf,rf)
}

######################################################

####bar plot of returns

#bind the return intv data together
return50base$CF <- "Historical"
return50future <- merge(return50future, WB_GCMs, by="GCM")
allreturns<-rbind(return50base, return50future)
allreturns$CF<-factor(allreturns$CF, levels=c("Historical",CFs))

#Bar graph 50-year return int for a 24-hour event
var_bar_plot(allreturns,"modeled", cols=colors3, title="Extreme precipitation 50-year recurrence interval", 
             ylab="Precipitation (inches/day)")
ggsave("Bar-50yrRecurrence.png", path=FigDir, width = PlotWidth, height = PlotHeight)


######line plot of return int regressions

#bind the regressions lines into one df
max100base$CF <- "Historical"
max100future <- merge(max100future, WB_GCMs, by="GCM")
allregressions<-rbind(max100base, max100future)
allregressions$CF<-factor(allregressions$CF, levels=c("Historical",CFs))

#line plots of regressions
ggplot(allregressions, aes(x=return, y=modeled, group=CF, colour = CF)) +
  geom_line(size = 2, stat = "identity",colour="black") + 
  geom_line(size = 1.5, stat = "identity") +
  geom_point(colour= "black", size=4, aes(fill = factor(CF), shape = factor(CF))) +
  PlotTheme + theme(axis.title.x=element_text(size=24, vjust=0.5,  margin=margin(t=20, r=20, b=20, l=20))) +
  labs(title = paste(SiteID, " - Recurrence intervals for 24-hour precipitation totals",sep=""),
       x = "Recurrence interval (year)", y = "Precipitation (inches/day)") +
  scale_color_manual(name="",values = colors3) +
  scale_fill_manual(name="",values = colors3) +
  scale_shape_manual(name="",values = c(21,22,23))
ggsave("line-recurrence-interval-curve.png", path=FigDir, width = PlotWidth, height = PlotHeight)

write.csv(allregressions, paste0(TableDir,"precip_recurrence_interval.csv"),row.names = FALSE)

