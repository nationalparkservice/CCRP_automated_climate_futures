NewColors <- c('#1b9e77','#d95f02','#7570b3') #selected from https://colorbrewer2.org/#type=qualitative&scheme=Dark2&n=3
SiteID = "TAPR"
DataDir <- "C:/Users/arunyon/3D Objects/Local-files/RCF_Testing/TAPR/TAPR/input-data/"

WB_GCMs %>% filter(CF %in% CFs) -> WB_GCMs

TableDir <- "C:/Users/arunyon/3D Objects/Local-files/RCF_Testing/TAPR/TAPR/WarmWet_HotDry/tables/"
FigDir <- "C:/Users/arunyon/3D Objects/Local-files/RCF_Testing/TAPR/TAPR/WarmWet_HotDry/figures/"
MonthlyWB <- read.csv(paste0(TableDir,"WB-Monthly.csv"))
AnnualWB <- read.csv(paste0(TableDir,"WB-Annual.csv"))

MonthlyWB <- MonthlyWB %>% mutate(Date = as.POSIXct(paste(substr(yrmon,1,4),substr(yrmon,5,6),"1",sep="-"),format="%Y-%m-%d"),
                                  year = format(Date, "%Y"))
MonthlyWB <- subset(MonthlyWB, year >= Yr-Range/2 & year <= Yr+Range/2 | year <= 2012)
# AnnualWB <- subset(AnnualWB, year >= Yr-Range/2 & year <= Yr+Range/2 | year <= 2012)
MonthlyWB<-merge(MonthlyWB,WB_GCMs,by="GCM", all.x=T)
MonthlyWB$CF[which(is.na(MonthlyWB$CF))] <- "Historical"
MonthlyWB$CF<-factor(MonthlyWB$CF, levels=c("Historical",CFs))
# MonthlyWB <- MonthlyWB %>%  drop_na()

# AnnualWB<-merge(AnnualWB,WB_GCMs,by="GCM", all.x=T)
# AnnualWB$CF<-factor(AnnualWB$CF, levels=c("Historical",CFs))
# AnnualWB <- AnnualWB %>% drop_na()

# Conversions to Imperial Units

#Annual Conversions
# AnnualWB_in <- AnnualWB %>% mutate(sum_snow.in = sum_snow.mm/ 25.4,
#                                    max_pack.in = max_pack.mm/ 25.4,
#                                    sum_pet.in = sum_pet.mm/ 25.4,
#                                    avg_soil.in=avg_soil.mm/ 25.4,
#                                    sum_aet.in = sum_aet.mm/ 25.4,
#                                    runoff.in = runoff.mm/ 25.4,
#                                    sum_d.in = sum_d.mm/ 25.4) 

#Monthly Conversions
MonthlyWB_in <- MonthlyWB %>% mutate(sum_snow.in = sum_snow.mm/ 25.4,
                                     max_pack.in = max_pack.mm/ 25.4,
                                     sum_pet.in = sum_pet.mm/ 25.4,
                                     avg_soil.in=avg_soil.mm/ 25.4,
                                     sum_aet.in = sum_aet.mm/ 25.4,
                                     runoff.in = runoff.mm/ 25.4,
                                     sum_d.in = sum_d.mm/ 25.4,
                                     sum_p.in = sum_p.mm/ 25.4) %>% 
  mutate(Month = substr(MonthlyWB$yrmon, 5, 7)) %>% group_by(CF, Month) %>% 
  summarise_at(vars(sum_snow.in,max_pack.in,sum_pet.in,avg_soil.in,sum_aet.in, runoff.in,sum_d.in,sum_p.in),mean) 

MonthlyWB_H <- subset(MonthlyWB_in, CF == "Historical")
MonthlyWB_delta = list()
split<-split(MonthlyWB_in,MonthlyWB_in$CF)
for(i in 1:length(split)){
  MD <- split[[i]]
  MD[,3:length(MD)] <- MD[,3:length(MD)] - MonthlyWB_H[,3:length(MonthlyWB_H)]
  MonthlyWB_delta[[i]] <- MD ; rm(MD)
}
MonthlyWB_delta<- ldply(MonthlyWB_delta, data.frame)
MonthlyWB_delta <- subset(MonthlyWB_delta, CF %in% CFs)
MonthlyWB_delta$CF<-droplevels(MonthlyWB_delta$CF)



### Monthly Plots
WBMonthlyLong <- MonthlyWB_in %>% select(.,-c("sum_snow.in","max_pack.in","avg_soil.in","sum_d.in","runoff.in")) %>% 
  rename(PET=sum_pet.in, AET=sum_aet.in, Ppt=sum_p.in) %>% 
  gather(Variable, water, -c(CF, Month)) 
WBMonthlyLong$Variable <- factor(WBMonthlyLong$Variable,levels = c("Ppt","PET","AET"))

WBplot <- function(scenario, cols){
  ggplot(MonthlyWB_in %>% filter(CF==scenario)) +
    geom_ribbon(aes(Month, ymin = sum_pet.in, ymax=sum_p.in,fill="Surplus/Runoff",group="CF"),linetype = 0, alpha=1) +
    geom_ribbon(aes(Month, ymin = sum_aet.in, ymax=sum_pet.in,fill="Deficit",group="CF"),linetype = 0,alpha=1) +
    geom_ribbon(aes(Month, ymin = 0, ymax=sum_aet.in,fill="Utilization",group="CF"),linetype = 0,alpha=1) +
    geom_line(data = WBMonthlyLong %>% filter(CF == scenario), aes(x=Month, y = water, group=Variable, linetype = Variable), linewidth = 1.5, stat = "identity",colour="black") +
    scale_fill_manual("",
                      values=c('Surplus/Runoff'=NewColors[3],'Utilization'=NewColors[1],'Deficit'=NewColors[2])) +
    scale_linetype_manual(values=c("solid","twodash", "dotted")) +
    labs(title = scenario) + PlotTheme + 
    theme(axis.title.x=element_blank(),axis.title.y=element_blank(),
          plot.background = element_rect(colour = cols, fill=NA, size=5)) +
    scale_x_discrete(labels = MonthLabels) +
    coord_cartesian(ylim = c(0, max(MonthlyWB_in[,c(5,7:10)]))) }

Hist.WBplot <- WBplot(scenario="Historical",cols="grey")
CF1.WBplot <- WBplot(scenario=CFs[1],cols=colors2[1])
CF2.WBplot <- WBplot(scenario=CFs[2],cols=colors2[2])

WBgrid <- ggarrange(Hist.WBplot, CF1.WBplot, CF2.WBplot, ncol = 3, nrow = 1,common.legend = T)
annotate_figure(WBgrid, left = textGrob("Water (in)", rot = 90, vjust = 1, gp = gpar(cex = 1.3)),
                bottom = textGrob("Month", gp = gpar(cex = 1.3)),
                top = textGrob(paste0(SiteID, " monthly water balance in ", Yr),
                               gp=gpar(fontface="bold", col="black",  fontsize=22)),
                fig.lab = "I",
                fig.lab.pos = "bottom.right")
ggsave("WaterBalance_Monthly_Horizontal.jpg", width = 15, height = 9, path = getwd(), bg="white")
