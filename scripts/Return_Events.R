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
Future_subset <- ALL_FUTURE %>% filter(GCM %in% WB_GCMs$GCM) %>%  left_join(WB_GCMs)
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
var_bar_plot(allreturns,"modeled", cols=colors3, title="50-year extreme precipitation (1:50) events", 
             ylab="Precipitation (inches/day)")
ggsave("50yr-PrecipEvent-bar.png", path=FigDir, width = PlotWidth, height = PlotHeight)


######line plot of return int regressions

#bind the regressions lines into one df
max100base$CF <- "Historical"
max100future <- merge(max100future, WB_GCMs, by="GCM")
allregressions<-rbind(max100base, max100future)
allregressions$CF<-factor(allregressions$CF, levels=c("Historical",CFs))

#line plots of regressions
RE <- ggplot(allregressions, aes(x=return, y=modeled, group=CF, colour = CF)) +
  geom_line(size = 2, stat = "identity",colour="black") + 
  geom_line(size = 1.5, stat = "identity") +
  geom_point(colour= "black", size=4, aes(fill = factor(CF), shape = factor(CF))) +
  PlotTheme + theme(axis.title.x=element_text(size=24, vjust=0.5,  margin=margin(t=20, r=20, b=20, l=20))) +
  labs(title = paste(SiteID, " - Recurrence intervals for 24-hour precipitation totals",sep=""),
       x = "Recurrence interval (year)", y = "Precipitation (inches/day)") +
  scale_color_manual(name="",values = colors3) +
  scale_fill_manual(name="",values = colors3) +
  scale_shape_manual(name="",values = c(21,22,23))
ggsave(plot=RE,"Precip-Return-Event-curve.png", path=FigDir, width = PlotWidth, height = PlotHeight)

RE + geom_vline(aes(xintercept=50),linetype=2,colour="black",size=1) +
  geom_segment(aes(x=1,xend=50,y=allreturns$modeled[which(allreturns$CF=="Historical")],yend=allreturns$modeled[which(allreturns$CF=="Historical")]),linetype=1,colour="grey",size=1) +
  geom_segment(aes(x=1,xend=50,y=allreturns$modeled[which(allreturns$CF==CFs[1])],yend=allreturns$modeled[which(allreturns$CF==CFs[1])]),linetype=1,colour=colors2[1],size=1) +
  geom_segment(aes(x=1,xend=50,y=allreturns$modeled[which(allreturns$CF==CFs[2])],yend=allreturns$modeled[which(allreturns$CF==CFs[2])]),linetype=1,colour=colors2[2],size=1)
ggsave("Precip-Return-Event-curve-50yr-lines.png", path=FigDir, width = PlotWidth, height = PlotHeight)

write.csv(allregressions, paste0(TableDir,"precip_recurrence_interval.csv"),row.names = FALSE)

Hist_return50 <- round(allreturns$modeled[which(allreturns$CF=="Historical")],0)
CF1_return50 <- allregressions %>% filter(CF == CFs[1]) %>% slice(which.min(abs(modeled - Hist_return50))) %>% select(return)
CF2_return50 <- allregressions %>% filter(CF == CFs[2]) %>% slice(which.min(abs(modeled - Hist_return50))) %>% select(return)

RE + geom_hline(aes(yintercept=Hist_return50),linetype=2,colour="black",size=1) + 
  geom_segment(aes(x=50,xend=50,y=0,yend=Hist_return50),linetype=1,colour="grey",size=1) +
  geom_segment(aes(x=CF1_return50[1,],xend=CF1_return50[1,],y=0,yend=Hist_return50),linetype=1,colour=colors2[1],size=1) +
  geom_segment(aes(x=CF2_return50[1,],xend=CF2_return50[1,],y=0,yend=Hist_return50),linetype=1,colour=colors2[2],size=1) 
ggsave("Precip-Return-Event-curve-prcp-lines.png", path=FigDir, width = PlotWidth, height = PlotHeight)

allregressions = allregressions %>% mutate(prob = 1/return)

ggplot(allregressions, aes(x=prob, y=modeled, group=CF, colour = CF)) +
  geom_line(size = 2, stat = "identity",colour="black") + 
  geom_line(size = 1.5, stat = "identity") +
  geom_point(colour= "black", size=4, aes(fill = factor(CF), shape = factor(CF))) +
  PlotTheme + theme(axis.title.x=element_text(size=24, vjust=0.5,  margin=margin(t=20, r=20, b=20, l=20))) +
  labs(title = paste(SiteID, " - Probability of 24-hour precipitation total",sep=""),
       x = "Probability of occurrence)", y = "Precipitation (inches/day)") +
  scale_color_manual(name="",values = colors3) +
  scale_fill_manual(name="",values = colors3) +
  scale_shape_manual(name="",values = c(21,22,23))
ggsave(plot=RE,"Precip-Probability-curve.png", path=FigDir, width = PlotWidth, height = PlotHeight)

write.csv(allregressions, paste0(TableDir,"precip_recurrence_interval.csv"),row.names = FALSE)
