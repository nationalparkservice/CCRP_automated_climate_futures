# RSS_Plot_Table_Creation vxx.R

################################################## INITIALS ##################################################
#Month and season names 
months=factor(c("January","February","March","April","May","June","July","August","September","October","November","December"),levels = month.name)
seasons=factor(c("Winter", "Spring", "Summer", "Fall"))
levels(seasons)=seasons


################################################### FUNCTION DEFINITIONS ########################################

#### FUNCTION TO CALCULATE SEASON FROM 'DATE' ####

getSeason.date <- function(DATES) {
  WS <- as.Date("2012-12-21", format = "%Y-%m-%d") # Winter Solstice
  SE <- as.Date("2012-3-21",  format = "%Y-%m-%d") # Spring Equinox
  SS <- as.Date("2012-6-21",  format = "%Y-%m-%d") # Summer Solstice
  FE <- as.Date("2012-9-21",  format = "%Y-%m-%d") # Fall Equinox
  
  # Convert dates from any year to 2012 dates
  d <- as.Date(strftime(DATES, format="2012-%m-%d"))
  
  ifelse (d >= WS | d < SE, "Winter",
          ifelse (d >= SE & d < SS, "Spring",
                  ifelse (d >= SS & d < FE, "Summer", "Fall")))
}

#### END FUNCTION ####

#### FUNCTION TO ASSIGN SEASON BY MONTH ####

getSeason <- function(MONTH) {
  MONTH = as.integer(MONTH)
  season = ifelse (MONTH > 2 & MONTH < 6, "Spring",
          ifelse (MONTH > 5 & MONTH < 9, "Summer",
                  ifelse (MONTH > 8 & MONTH < 12, "Fall", "Winter")))
  return(season)
}

#### END FUNCTION ####


#### FUNCTION TO CALCULATE AVERAGE OF TOTAL DAYS/YEAR FOR A VARIABLE ####

MeanAnnualTotals = function(DF, VarName){
  Years = length(unique(strftime(DF$Date, "%Y")))
  VarIndex = which(colnames(DF) == VarName)
  MeanAnnualTotal = aggregate(DF[,VarIndex] ~ DF$GCM, FUN=function(x){sum(x)/Years}) 
  names(MeanAnnualTotal) = c("GCM", "MeanAnnualTotals")
  return(MeanAnnualTotal)
}

#### END FUNCTION ####


#### FUNCTION TO CALCULATE AVERAGE OF MAXIMUM ANNUAL DAYS/YEAR FOR A VARIABLE ####
MeanAnnualMax = function(DF, VarName){
  VarIndex = which(colnames(DF) == VarName)
  YearlyMax = aggregate(DF[,VarIndex], by=list(GCM=DF$GCM, Year=DF$Date$year), FUN=max)
  MeanAnnualMax = aggregate(YearlyMax[,3] ~ YearlyMax$GCM, FUN=mean)
  names(MeanAnnualMax) = c("GCM", "MeanAnnualMax")
  return(MeanAnnualMax)
}

#### END FUNCTION ####


#### FUNCTION TO COMPARE BASELINE TO FUTURE MEANS AND CALCULATE DELTAS ####

GetAnnualMeanDeltas = function(BaseMeans, FutureMeans){
  TotalMeans = merge(BaseMeans, FutureMeans, by="GCM")
  TotalMeans$Delta = unlist(FutureMeans[2] - BaseMeans[2])
  return(TotalMeans)
}

#### END FUNCTION ####


#### FUNCTION TO CALCULATE AVERAGE OF MAXIMUM SEASONAL DAYS/YEAR FOR A VARIABLE ####
MeanSeasonalMax = function(DF, VarName){
  VarIndex = which(colnames(DF) == VarName)
  YearlyMax = aggregate(DF[,VarIndex], by=list(GCM=DF$GCM, Year=DF$Date$year, Season=DF$season), FUN=max)
  MeanAnnualMax = aggregate(YearlyMax[,4], by=list(GCM=YearlyMax$GCM, Season=YearlyMax$Season), FUN=mean)
  names(MeanAnnualMax) = c("GCM", "Season", "MeanAnnualMax")
  return(MeanAnnualMax)
}

#### END FUNCTION ####


#### FUNCTION TO COMPARE BASELINE TO FUTURE MEANS AND CALCULATE DELTAS ####

GetSeasonalMeanDeltas = function(BaseMeans, FutureMeans){
  TotalMeans = merge(BaseMeans, FutureMeans, by=c("GCM", "Season"))
  TotalMeans$Delta = unlist(FutureMeans[3] - BaseMeans[3])
  return(TotalMeans)
}

#### END FUNCTION ####

#### FUNCTION TO CALCULATE HEAT INDEX ####
#use daily tmax for temp as well as rhs_max
heat_index <- function(temp, RH) {
  Sted <- 0.5 * (temp + 61 + ((temp - 68) * 1.2) + (RH * 0.094))
  Roth <- -42.379 + (2.04901523 * temp) + (10.14333127 * RH) + (-.22475541 * temp * RH) +
    (-.00683783 * temp^2) + (-.05481717 * RH^2) + (.00122874 * temp^2 * RH) + 
    (.00085282 * temp * RH^2) + (-.00000199 * temp^2 * RH^2)
  adj1 <- ((13 - RH) / 4) * sqrt((17 - abs(temp - 95)) / 17)
  adj2 <- ((RH - 85) / 10) * ((87 - temp) / 5)
  heat_index<-ifelse(temp < 80, Sted, 
                     ifelse(RH < 13 & temp > 80 & temp < 112, Roth-adj1,
                            ifelse(RH > 85 & temp > 80 & temp < 87, Roth+adj2, Roth)))
  heat_index
} #creates errors but doesn't matter becuase not used when not applicable

#### END FUNCTION ####

################################ END FUNCTION DEFINITIONS #########################################

################################################### SUBSET TIME PERIOD ########################################
Baseline_all <- Gridmet
Future_all <- ALL_FUTURE

# # Subset Future_all to only be near future (2025-2055) and Baseline_all to only but until 2000


Baseline_all<-subset(Baseline_all,Year<2013)
Future_all = subset(Future_all, Year >= Yr - (Range/2) & Year <= (Yr + (Range/2)) & GCM %in% custom_GCMs)

################################# SUMMARIZE CHANGE IN FUTURE TEMP/PRECIP MEANS BY GCM ####################
####Set Average values for all four weather variables, using all baseline years and all climate models
BaseMeanPr = mean(Baseline_all$PrcpIn)
BaseMeanTmx = mean(Baseline_all$TmaxF)
BaseMeanTmn = mean(Baseline_all$TminF)

####Create Future/Baseline means data tables, with averages for all four weather variables, organized by GCM
Future_Means = aggregate(cbind(PrcpIn, TmaxF, TminF, TavgF)
                                    ~ GCM, Future_all, mean,na.rm=FALSE)   # , Future_all$Wind
# names(Future_Means) = c("GCM", "PrcpIn", "TmaxF", "TminF", "TavgF")    # , "Wind"

Baseline_Means = aggregate(cbind(PrcpIn, TmaxF, TminF, TavgF)~GCM, 
                                      Baseline_all, mean)   
# names(Baseline_Means) = c("GCM", "PrcpIn", "TmaxF", "TminF", "TavgF") 

#### add delta columns in order to classify CFs
Future_Means <- Future_Means %>% mutate(DeltaPr = PrcpIn - Baseline_Means$PrcpIn,
                                        DeltaTmx = TmaxF - Baseline_Means$TmaxF,
                                        DeltaTmn = TminF - Baseline_Means$TminF,
                                        DeltaTavg = TavgF -  Baseline_Means$TavgF) %>% 
  left_join(WB_GCMs,by="GCM")


####Add column with CF classification to Future_all/Baseline_all
CF_GCM = data.frame(GCM = Future_Means$GCM, CF = Future_Means$CF)
Future_all = merge(Future_all, CF_GCM[1:2], by="GCM")
Baseline_all$CF = "Historical"

################################ SUMMARIZE TEMPERATURE, PRECIP, RH BY MONTH & SEASON #######################
Baseline_all$Month<-format(Baseline_all$Date,"%m")
Baseline_all$Year<-format(Baseline_all$Date,"%Y")
Future_all$Month<-format(Future_all$Date,"%m")
Future_all$Year<-format(Future_all$Date,"%Y")

#Add season variable for all data in Future_all and Baseline_all
Future_all$season=getSeason(Future_all$Month)
Baseline_all$season=getSeason(Baseline_all$Month)

#### Create tables with monthly tmax/tmin/tmean/precip/RHmean delta by CF
# Historical
Tmax = aggregate(TmaxF~Month+GCM+CF,Baseline_all,mean,na.rm=TRUE)
Tmin = aggregate(TminF~Month+GCM+CF,Baseline_all,mean,na.rm=TRUE)
Tmean = aggregate(TavgF~Month+GCM+CF,Baseline_all,mean,na.rm=TRUE)
Precip = aggregate(PrcpIn~Month+Year+GCM+CF,Baseline_all,sum,na.rm=TRUE)
Precip = aggregate(PrcpIn~Month+GCM+CF,Precip,mean,na.rm=TRUE)
Baseline_all$RHmean<-(Baseline_all$RHmaxPct+Baseline_all$RHminPct)/2
RHmean = aggregate(RHmean~Month+GCM+CF,Baseline_all,mean,na.rm=TRUE)
VPD = aggregate(VPD~Month+GCM+CF,Baseline_all,mean,na.rm=TRUE)

H_Monthly<-Reduce(function(...)merge(...,all=T),list(Tmax,Tmin,Tmean,Precip,RHmean,VPD))
rm(Tmax,Tmin,Tmean,Precip,RHmean,VPD)

# Future
Tmax = aggregate(TmaxF~Month+GCM+CF,Future_all,mean,na.rm=TRUE)
Tmin = aggregate(TminF~Month+GCM+CF,Future_all,mean,na.rm=TRUE)
Tmean = aggregate(TavgF~Month+GCM+CF,Future_all,mean,na.rm=TRUE)
Precip = aggregate(PrcpIn~Month+Year+GCM+CF,Future_all,sum,na.rm=TRUE)
Precip = aggregate(PrcpIn~Month+GCM+CF,Precip,mean,na.rm=TRUE)
Future_all$RHmean<-(Future_all$RHmaxPct+Future_all$RHminPct)/2
RHmean = aggregate(RHmean~Month+GCM+CF,Future_all,mean,na.rm=TRUE)
VPD= aggregate(VPD~Month+GCM+CF,Future_all,mean,na.rm=TRUE)

F_Monthly<-Reduce(function(...)merge(...,all=T),list(Tmax,Tmin,Tmean,Precip,RHmean,VPD))
rm(Tmax,Tmin,Tmean,Precip,RHmean,VPD)


#### Create tables with seasonal tmax/tmin/tmean/precip/RHmean delta by CF
# Historical
Tmax = aggregate(TmaxF~season+GCM+CF,Baseline_all,mean,na.rm=TRUE)
Tmin = aggregate(TminF~season+GCM+CF,Baseline_all,mean,na.rm=TRUE)
Tmean = aggregate(TavgF~season+GCM+CF,Baseline_all,mean,na.rm=TRUE)
Precip = aggregate(PrcpIn~season+Year+GCM+CF,Baseline_all,sum,na.rm=TRUE)
Precip = aggregate(PrcpIn~season+GCM+CF,Precip,mean,na.rm=TRUE)
Baseline_all$RHmean<-(Baseline_all$RHmaxPct+Baseline_all$RHminPct)/2
RHmean = aggregate(RHmean~season+GCM+CF,Baseline_all,mean,na.rm=TRUE)
VPD = aggregate(VPD~season+GCM+CF,Baseline_all,mean,na.rm=TRUE)

H_Season<-Reduce(function(...)merge(...,all=T),list(Tmax,Tmin,Tmean,Precip,RHmean,VPD))
rm(Tmax,Tmin,Tmean,Precip,RHmean,VPD)

# Future
Tmax = aggregate(TmaxF~season+GCM+CF,Future_all,mean,na.rm=TRUE)
Tmin = aggregate(TminF~season+GCM+CF,Future_all,mean,na.rm=TRUE)
Tmean = aggregate(TavgF~season+GCM+CF,Future_all,mean,na.rm=TRUE)
Precip = aggregate(PrcpIn~season+Year+GCM+CF,Future_all,sum,na.rm=TRUE)
Precip = aggregate(PrcpIn~season+GCM+CF,Precip,mean,na.rm=TRUE)
Future_all$RHmean<-(Future_all$RHmaxPct+Future_all$RHminPct)/2
RHmean = aggregate(RHmean~season+GCM+CF,Future_all,mean,na.rm=TRUE)
VPD = aggregate(VPD~season+GCM+CF,Future_all,mean,na.rm=TRUE)

F_Season<-Reduce(function(...)merge(...,all=T),list(Tmax,Tmin,Tmean,Precip,RHmean,VPD))
rm(Tmax,Tmin,Tmean,Precip,RHmean,VPD)


################################ SUMMARIZE TEMPERATURE AND PRECIP BY MONTH & SEASON #######################

# Monthly abs
H_MonMean<-aggregate(cbind(TmaxF,TminF,TavgF,PrcpIn,RHmean,VPD)~Month,H_Monthly,mean)
H_MonMean$CF<-"Historical";H_MonMean<-H_MonMean[,c("Month","CF",names(H_MonMean[,2:7]))]
F_MonCF<-aggregate(cbind(TmaxF,TminF,TavgF,PrcpIn,RHmean,VPD)~Month+CF,F_Monthly,mean)
Monthly<-rbind(H_MonMean,F_MonCF)
Monthly$CF<-factor(Monthly$CF,levels = c(FutureSubset,"Historical"))

# Monthly delta
Monthly_delta<-F_MonCF
for (i in 3:8){
  Monthly_delta[,i]<-F_MonCF[,i]-H_MonMean[,i][match(F_MonCF$Month,H_MonMean$Month)]
}
Monthly_delta$CF<-factor(Monthly_delta$CF,levels = c(FutureSubset))
Monthly_delta$PrcpPct <- (Monthly_delta$PrcpIn/H_MonMean$PrcpIn)*100

# Seasonal abs
H_SeasMean<-aggregate(cbind(TmaxF,TminF,TavgF,PrcpIn,RHmean,VPD)~season,H_Season,mean)
H_SeasMean$CF<-"Historical";H_SeasMean<-H_SeasMean[,c("season","CF",names(H_SeasMean[,2:7]))]
F_SeasCF<-aggregate(cbind(TmaxF,TminF,TavgF,PrcpIn,RHmean,VPD)~season+CF,F_Season,mean)
Season<-rbind(H_SeasMean,F_SeasCF)
Season$CF<-factor(Season$CF,levels = c(FutureSubset,"Historical"))
Season$season = factor(Season$season, levels = c("Winter","Spring","Summer","Fall"))

# Season delta
Season_delta<-F_SeasCF
for (i in 3:8){
  Season_delta[,i]<-F_SeasCF[,i]-H_SeasMean[,i][match(F_SeasCF$season,H_SeasMean$season)]
}; Season_delta$CF<-factor(Season_delta$CF,levels = c(FutureSubset))
Season_delta$season = factor(Season_delta$season, levels = c("Winter","Spring","Summer","Fall"))
Season_delta$PrcpPct <- (Season_delta$PrcpIn/H_SeasMean$PrcpIn)*100

########################################## END MONTH & SEASON SUMMARY ##########################################


######################################## CALCULATE ANNUAL DAYS ABOVE/BELOW TEMP & PRECIP THRESHOLDS ##########################

###### TOTAL & CONSECUTIVE DAYS OVER/UNDER THRESHOLD TEMPS ######
Baseline_all[order("Date","GCM"),]
Future_all[order("Date","GCM"),]

HistYears = length(unique(Baseline_all$Date$year))

HistTmax99 = quantile(Baseline_all$TmaxF, 0.99)
HistTmaxHigh = quantile(Baseline_all$TmaxF, QuantileHigh)
HistTminLow = quantile(Baseline_all$TminF, QuantileLow)
HistPrecip95 = quantile(Baseline_all$PrcpIn[which(Baseline_all$PrcpIn > 0.05)], 0.95) #percentil of days receiving precip
HistPr99 = quantile(Baseline_all$PrcpIn[which(Baseline_all$PrcpIn > 0.05)], 0.99)

Baseline_all$Julian = Baseline_all$Date$yday
Baseline_all$halfyr = ifelse(Baseline_all$Julian<=182,1,2)
Baseline_all<-Baseline_all[with(Baseline_all,order(Year,GCM,Julian)),]
Baseline_all$TavgF = (Baseline_all$TmaxF + Baseline_all$TminF)/2
Baseline_all$OverHotTemp = Baseline_all$TmaxF > HotTemp
Baseline_all$OverHighQ = Baseline_all$TmaxF > HistTmaxHigh
Baseline_all$Tmax99 = Baseline_all$TmaxF > HistTmax99
Baseline_all$HeatConsecutive=(Baseline_all$Tmax99)*unlist(lapply(rle(Baseline_all$Tmax99)$lengths, seq_len))
Baseline_all$UnderColdTemp = Baseline_all$TminF < ColdTemp
Baseline_all$UnderLowQ = Baseline_all$TminF < HistTminLow
Baseline_all$HeatConsecutive=(Baseline_all$OverHotTemp)*unlist(lapply(rle(Baseline_all$OverHotTemp)$lengths, seq_len))
Baseline_all$ColdConsecutive=(Baseline_all$UnderColdTemp)*unlist(lapply(rle(Baseline_all$UnderColdTemp)$lengths, seq_len))
Baseline_all$NoPrecip = Baseline_all$PrcpIn < PrecipThreshold
Baseline_all$NoPrecipLength = (Baseline_all$NoPrecip)*unlist(lapply(rle(Baseline_all$NoPrecip)$lengths, seq_len)) 
Baseline_all$OverPrecip95 = Baseline_all$PrcpIn > HistPrecip95
Baseline_all$OverPrecip99 = Baseline_all$PrcpIn > HistPr99
Baseline_all$PrecipOver1 = Baseline_all$PrcpIn > 1
Baseline_all$PrecipOver2 = Baseline_all$PrcpIn > 2
Baseline_all$FThaw = Baseline_all$TminF<28 & Baseline_all$TmaxF>34
Baseline_all$GDD = Baseline_all$TavgF>41 # 5 deg C
Baseline_all %>% 
  group_by(GCM, idx = cumsum(GDD == 0L)) %>% 
  mutate(GDD_count = row_number()) %>% 
  ungroup %>% 
  select(-idx) -> Baseline_all

Baseline_all %>% 
  group_by(GCM, idx = cumsum(GDD == 1L)) %>% 
  mutate(N_GDD_count = row_number()) %>% 
  ungroup %>% 
  select(-idx) -> Baseline_all
Baseline_all$HI = heat_index(Baseline_all$TmaxF,Baseline_all$RHminPct)
Baseline_all$HI.EC = Baseline_all$HI >89 & Baseline_all$HI <103
Baseline_all$HI.Dan = Baseline_all$HI >102 & Baseline_all$HI < 124
Baseline_all$Frost = Baseline_all$GDD == TRUE & Baseline_all$TminF < 32

Future_all$Julian = Future_all$Date$yday
Future_all$halfyr = ifelse(Future_all$Julian<=182,1,2)
Future_all<-Future_all[with(Future_all,order(Year,GCM,Julian)),]
Future_all$TavgF = (Future_all$TmaxF + Future_all$TminF)/2
Future_all$OverHotTemp = Future_all$TmaxF > HotTemp
Future_all$OverHighQ = Future_all$TmaxF > HistTmaxHigh
Future_all$Tmax99 = Future_all$TmaxF > HistTmax99
Future_all$HeatConsecutive=(Future_all$Tmax99)*unlist(lapply(rle(Future_all$Tmax99)$lengths, seq_len))
Future_all$UnderColdTemp = Future_all$TminF < ColdTemp
Future_all$UnderLowQ = Future_all$TminF < HistTminLow
Future_all$HeatConsecutive=(Future_all$OverHotTemp)*unlist(lapply(rle(Future_all$OverHotTemp)$lengths, seq_len))
Future_all$ColdConsecutive=(Future_all$UnderColdTemp)*unlist(lapply(rle(Future_all$UnderColdTemp)$lengths, seq_len))
Future_all$NoPrecip = Future_all$PrcpIn < PrecipThreshold
Future_all$NoPrecipLength = (Future_all$NoPrecip)*unlist(lapply(rle(Future_all$NoPrecip)$lengths, seq_len)) 
Future_all$OverPrecip95 = Future_all$PrcpIn > HistPrecip95
Future_all$OverPrecip99 = Future_all$PrcpIn > HistPr99
Future_all$PrecipOver1 = Future_all$PrcpIn > 1
Future_all$PrecipOver2 = Future_all$PrcpIn > 2
Future_all$FThaw = Future_all$TminF<28 & Future_all$TmaxF>34
Future_all$GDD = Future_all$TavgF>41 # 5 deg C

Future_all %>% 
  group_by(GCM, idx = cumsum(GDD == 0L)) %>% 
  mutate(GDD_count = row_number()) %>% 
  ungroup %>% 
  select(-idx) -> Future_all

Future_all %>% 
  group_by(GCM, idx = cumsum(GDD == 1L)) %>% 
  mutate(N_GDD_count = row_number()) %>% 
  ungroup %>% 
  select(-idx) -> Future_all

Future_all$HI = heat_index(Future_all$TmaxF,Future_all$RHminPct)
Future_all$HI.EC = Future_all$HI >89 & Future_all$HI <103
Future_all$HI.Dan = Future_all$HI >102 & Future_all$HI < 124
Future_all$Frost = Future_all$GDD == TRUE & Future_all$TminF < 32

#### Historical Dataframes aggregated by Year+GCM ###########
H_annual <- Baseline_all %>% group_by(CF, GCM, Year) %>%
  summarise_at(vars(PrcpIn,OverHotTemp, OverHighQ, Tmax99, UnderColdTemp,UnderLowQ,  
                    NoPrecip, NoPrecipLength, OverPrecip95, OverPrecip99, PrecipOver1, PrecipOver2,
                    FThaw, GDD,HI.EC,HI.Dan), sum)  

Hmeans<-Baseline_all %>% group_by(CF, GCM, Year) %>%
  summarise_at(vars(TmaxF,TminF,TavgF,RHmean),mean)
H_annual<-merge(H_annual,Hmeans,by=c("CF","GCM","Year"), all=TRUE);rm(Hmeans)

# Agrregate mean w/ temps only W months
H.WinterTemp<- Baseline_all %>% group_by(CF, GCM, Year) %>% 
  filter(Month<3 | Month>11) %>% 
  summarise(W.Temp=mean(TavgF)) 
H_annual <- merge(H_annual,H.WinterTemp,by=c("CF","GCM","Year")); rm(H.WinterTemp)


# # Further Growing Season Calculations
Baseline_GS <- as.data.table(subset(Baseline_all,select=c(Year,CF,GCM,Julian,GDD_count,N_GDD_count,halfyr)))
Baseline_GS[order("Date","GCM"),]
Baseline_GU <- Baseline_GS %>% group_by(CF, GCM, Year) %>% 
  filter(GDD_count ==7, halfyr == 1) %>% 
  slice(1) %>% # takes the first occurrence if there is a tie
  ungroup()

Baseline_SE <- Baseline_GS %>% group_by(CF, GCM, Year) %>% 
  filter(N_GDD_count ==6, halfyr == 2) %>% 
  slice(1) %>% # takes the first occurrence if there is a tie
  ungroup()
Baseline_SE$"EndGrow"<-Baseline_SE$Julian - 6

H<-Baseline_GU %>% group_by(CF, GCM, Year) %>%
  summarise(BegGrow = mean(Julian))
H<-merge(H,Baseline_SE[,c("CF","GCM","Year","EndGrow")],by=c("CF","GCM","Year"), all=TRUE)
H$EndGrow[is.na(H$EndGrow)]<-365
H$GrowLen<- H$EndGrow - H$BegGrow
H_annual<-merge(H_annual,H,by=c("CF","GCM","Year"), all=TRUE)
rm(Baseline_GS,Baseline_GU,Baseline_SE,H)

# Frost length calculations - late spring freeze events
Sp.Frost<-Baseline_all %>% group_by(CF, GCM, Year) %>%
  filter(Julian < 180) %>% 
  summarise(Sp.Frost = sum(Frost))
H_annual<-merge(H_annual,Sp.Frost,by=c("CF","GCM","Year"), all=TRUE);rm(Sp.Frost)



#### Future Dataframes aggregated by Year+GCM ###########
F_annual <- Future_all %>% group_by(CF, GCM, Year) %>%
  summarise_at(vars(PrcpIn,OverHotTemp, OverHighQ, Tmax99, UnderColdTemp,UnderLowQ,  
                    NoPrecip, NoPrecipLength, OverPrecip95, OverPrecip99, PrecipOver1, PrecipOver2,
                    FThaw, GDD,HI.EC,HI.Dan), sum)  
  
Fmeans<-Future_all %>% group_by(CF, GCM, Year) %>%
  summarise_at(vars(TmaxF,TminF,TavgF,RHmean),mean)
F_annual<-merge(F_annual,Fmeans,by=c("CF","GCM","Year"), all=TRUE);rm(Fmeans)

# Agrregate mean w/ temps only W months
F.WinterTemp<- Future_all %>% group_by(CF, GCM, Year) %>% 
  filter(Month<3 | Month>11) %>% 
  summarise(W.Temp=mean(TavgF)) 
F_annual <- merge(F_annual,F.WinterTemp,by=c("CF","GCM","Year")); rm(F.WinterTemp)


# # Further Growing Season Calculations
Future_GS <- as.data.table(subset(Future_all,select=c(Year,CF,GCM,Julian,GDD_count,N_GDD_count,halfyr)))
Future_GS[order("Date","GCM"),]
Future_GU <- Future_GS %>% group_by(CF, GCM, Year) %>% 
  filter(GDD_count ==7, halfyr == 1) %>% 
  slice(1) %>% # takes the first occurrence if there is a tie
  ungroup()

Future_SE <- Future_GS %>% group_by(CF, GCM, Year) %>% 
  filter(N_GDD_count ==6, halfyr == 2) %>% 
  slice(1) %>% # takes the first occurrence if there is a tie
  ungroup()
Future_SE$"EndGrow"<-Future_SE$Julian - 6

F<-Future_GU %>% group_by(CF, GCM, Year) %>%
  summarise(BegGrow = mean(Julian))
F<-merge(F,Future_SE[,c("CF","GCM","Year","EndGrow")],by=c("CF","GCM","Year"), all=TRUE)
F$EndGrow[is.na(F$EndGrow)]<-365
F$GrowLen<- F$EndGrow - F$BegGrow
F_annual<-merge(F_annual,F,by=c("CF","GCM","Year"), all=TRUE)
rm(Future_GS,Future_GU,Future_SE,F)

# Frost length calculations - late spring freeze events
Sp.Frost<-Future_all %>% group_by(CF, GCM, Year) %>%
  filter(Julian < 180) %>% 
  summarise(Sp.Frost = sum(Frost))
F_annual<-merge(F_annual,Sp.Frost,by=c("CF","GCM","Year"), all=TRUE);rm(Sp.Frost)


######################################## END THRESHOLD CALCULATIONS ##############################
write.csv(Future_Means, paste0(DataDir,SiteID,"_Future_Means-custom-CFs.csv"),row.names = F)
