library(climateR)
library(AOI)
library(dplyr)
library(ggplot2)

rm(list=ls())

# Needs aoi to run -- dig back through original cft code at how to create aoi from lat/lon
# AOI<-geocode(location = c("Fort Collins"), pt = TRUE) 
AOI<- aoi_get(list(42.735558, -99.743792,.01,.01)) #Coordiantes of east NIOB location

vars = c("tasmax", "tasmin", "pr", "rhsmax", "rhsmin")

TFtoC <- function(T){(T-32)/1.8}

# VP from FAO -  https://www.fao.org/3/x0490e/x0490e07.htm
# could also use Buck 1981 for 'improved':
# Buck: VPDsat (mb) = (1.0007 + (3.46 * 10^-6 * P)) * 6.1121 * exp((17.50 * T)/(T+240.87))
# where T is deg C and P is atm pressure mb. Above for P > 800 (correction is minimal)
# Zackman re: Ragwala uses: Es = 611.6441 * 10^[(7.591386*T)/(240.7263+T)] where Tavg.
#   Shelley sent Vaisala- to use Tavg for 611.6441 parameter - for 020 to +50 C.
# VPsatT = saturation VP @ T deg C [kPa]
# VPD [kPa]
VPsatT <- function(T){0.6108 * exp((17.27 * T)/(T + 237.3))}   

VPD <- function(TminF, TmaxF, RHmin, RHmax){
  Tmin <- TFtoC(TminF); Tmax <- TFtoC(TmaxF)
  es <- (VPsatT(Tmin)+VPsatT(Tmax))/2
  ea <- (VPsatT(Tmin)*RHmax*.01 + VPsatT(Tmax)*RHmin*.01)/2
  es - ea   }  # end VPD  
# scens = c("rcp45", "rcp85")

start.time <-Sys.time()
future_all <- data.frame()
for (i in 1:length(vars)){
  future1 = getMACA(AOI, 
                    model = 18, varname = vars[i], scenario  = "rcp45",
                    startDate = "2023-01-01", endDate = "2099-12-31")
  future2 = getMACA(AOI, 
                    model = 18, varname = vars[i], scenario  = "rcp85",
                    startDate = "2023-01-01", endDate = "2099-12-31")
  future<- left_join(future1, future2, by="date")
  
  future_long = future |>  
    tidyr::pivot_longer(-date)
  FL <- future_long |> 
    mutate(GCM = gsub("^[^_]*_([^_]+)_.*$", "\\1", future_long$name),
           RCP = sub('.*_', '', future_long$name)) |> 
    rename(!!vars[i]:=value) |> select(-c(name))
  if(i==1) { future_all = FL } else {
    future_all = left_join(future_all, FL, by=c("date","GCM","RCP"))
    rm(future_long, FL,future1, future2, future)
  }
}
end.time <- Sys.time()
end.time-start.time

# future_all <- read.csv("future_ClimateR.csv",header=T)
# future_all$date <- as.POSIXct(future_all$date,format="%Y-%m-%d")

Future_all <- future_all |> mutate(Date=date,
                          GCM=paste(GCM,RCP,sep="."),
                          PrcpIn = pr/25.4,
                          TmaxF = ((tasmax-273.15)*9/5) + 32,
                          TminF = ((tasmin-273.15)*9/5) + 32,
                          RHmaxPct = rhsmax,
                          RHminPct = rhsmin,
                          TavgF = (TmaxF+TminF)/2,
                          Year = format(date,"%Y")) |> 
  select(c("Date","GCM","PrcpIn","TmaxF","TminF","RHmaxPct","RHminPct","TavgF","Year", "RCP")) |> 
  mutate(VPD = VPD(TminF, TmaxF, RHminPct, RHmaxPct),
         DOY = yday(Date))

write.csv(Future_all,str_c(DataDir,SiteName,"_future.csv", sep=''),row.names = F)

####################
#### GRIDMET data
vars = c("tmmx", "tmmn", "pr", "rmax", "rmin")

historical_all <- data.frame()
for (i in 1:length(vars)){
 hist = getGridMET(AOI, varname = vars[i], startDate = "1979-01-01", endDate = "2022-12-31")
  HL <- hist
  if(i==1) { historical_all = HL } else {
    historical_all = left_join(historical_all, HL, by=c("date"))
    rm(HL,hist)
 }
}

Gridmet <- historical_all |> mutate(Date=date,
                                   GCM="gridmet.historical",
                                   PrcpIn = pr/25.4,
                                   TmaxF = ((tmmx-273.15)*9/5) + 32,
                                   TminF = ((tmmn-273.15)*9/5) + 32,
                                   RHmaxPct = rmax,
                                   RHminPct = rmin,
                                   TavgF = (TmaxF+TminF)/2,
                                   Year = format(date,"%Y")) |> 
  select(c("Date","GCM","PrcpIn","TmaxF","TminF","RHmaxPct","RHminPct","TavgF","Year")) |> 
  mutate(VPD = VPD(TminF, TmaxF, RHminPct, RHmaxPct),
         DOY = yday(Date))

write.csv(Gridmet,str_c(DataDir,SiteName,"_historical.csv", sep=''),row.names = F)
