# Uses terra functions to loop through list of sites and download data
# Format c(SiteID, Lat, Lon)
library(ncdf4)
library(reshape2)
library(tidyr)
library(lubridate)
library(stars)
library(sf)
# library(raster)
library(terra)
library(dplyr)
############################# User-defined initials #################################
rm(list=ls())

OutDir <- "C:/Users/arunyon/3D Objects/Local-files/RCF_Testing/MACA-test/"

StartYear <- 2024
EndYear <- 2099

# site.data <- read.csv("C:/Users/arunyon/Downloads/Sites.csv") #load csv with sites
site.data <- data.frame(SiteID=c("BAWA","SUIT"),Lat=c(-76.85247395,-76.91112483),Lon=c(39.0332476,38.83428703)) #Enter manually
site.data$cLon = site.data$Lon + 360 #Adjusts negative lon. values


data <- data.frame(SiteID=site.data$SiteID, Lat=site.data$Lat,Lon=site.data$cLon)
shp <- st_as_sf(data, coords = c("Lon","Lat"))
st_crs(shp) <- 4326
buff<-st_buffer(shp, 10)
v <- vect(buff)
s <- shift(v, 360) #need to convert shapefile to 0-360 (faster than converting each .nc file)

#Variable and scenario names corresponding to MACA data directory structure
vars = c("pr", "tasmax", "tasmin","rhsmax","rhsmin")
RCPs = c("rcp45", "rcp85")

# MACADir <- "C:/Users/arunyon/3D Objects/Local-files/RCF_Testing/MACA-test/MACAv2Metdata" #No / at end
MACADir <- "D:/ClimateData/MACAv2Metdata" #No / at end

start.time <- Sys.time()

final.extract <- data.frame()
gcm.all <- data.frame()
for (R in 1:length(RCPs)){
  for (v in 1:length(vars)){ #(v in 1:2){
    print(paste("Extracting", RCPs[R], vars[v]))
    path = paste(MACADir, RCPs[R], vars[v], sep = '/')
    names <- list.files(path = path, pattern = '.nc', full.names = FALSE)
    x <- strsplit(names, "_")
    index <- list()
    for (i in 1:length(x)){
      if(x[[i]][7]>=StartYear & x[[i]][6]<=EndYear) {index <- c(index, i)}
    }
    names <- names[unlist(index)]
    file.list = paste(path,names,sep="/")
    nc.data <- data.frame()
    for (i in 1:length(file.list)){
      raster.obj = terra::rast(file.list[i])
      crs(raster.obj)  <- "epsg:4326" 
      for (S in 1:length(s)){
        site <- s[S]
        print(site)
      raster.extract<-terra::extract(raster.obj, site)
      raster.extract$ID <- site$SiteID
      colnames(raster.extract)[2:length(raster.extract)] = as.character(time(raster.obj))
      split <- strsplit(names[i], "_")[[1]]
      raster.extract <- raster.extract |> 
        pivot_longer(!ID, names_to = "Date", values_to = vars[v]) #numbers correspond with vector length, if filename changes need to update
      raster.extract$gcm = split[3]
      raster.extract$rcp = RCPs[R]
      nc.data <- rbind(nc.data,raster.extract)
      print(paste(split[3],RCPs[R], "extracted"))
      }
    }
    if(v==1){gcm.all=nc.data} else{gcm.all=full_join(gcm.all,nc.data,by=c("ID","Date","gcm","rcp"))} 
  }
  final.extract <- rbind(final.extract,gcm.all)
}

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

# Split by ID and save as separate .csv files -- NEED TO CHANGE FILE NAMES TO FINAL.EXTRACT

ids <- unique(final.extract$ID)
for(i in 1:length(ids)){
final.extract |> filter(ID == ids[i]) |> 
  mutate(PrcpIn = pr/25.4,
            TmaxF = (tasmax * (9/5)) - 459.67,
            TminF = (tasmin * (9/5)) - 459.67,
            RHmaxPct = rhsmax,
            RHminPct = rhsmin,
            TavgF = (TmaxF+TminF)/2,
            Year = year(Date),
            GCM = paste(gcm,rcp,sep=".")) |> 
  select(Date,GCM,PrcpIn, TmaxF, TminF, RHmaxPct, RHminPct, TavgF, Year) |> 
write.csv(file=paste0(OutDir,ids[i],"_future.csv"),row.names=FALSE)
}



##################################################################################
#### GRIDMET data
GridDir <- "D:/ClimateData/gridmet"
vars = c("precip", "tmax", "tmin", "rmax","rmin") #folder names

final.extract <- data.frame()
for (v in 1:length(vars)){ #(v in 1:2){
  path = paste(GridDir, vars[v], sep = '/')
  names <- list.files(path = path, pattern = '.nc', full.names = FALSE)
  file.list = paste(path,names,sep="/")
  nc.data <- data.frame()
  for (i in 1:length(file.list)){
    raster.obj = terra::rast(file.list[i])
    crs(raster.obj)  <- "epsg:4326" 
    for (S in length(buff)){
      site <- buff[S]
    raster.extract<-terra::extract(raster.obj, site)
    raster.extract$ID <- site$SiteID
    colnames(raster.extract)[2:length(raster.extract)] = as.character(as.Date("1900-01-01") + as.numeric(substr(names(raster.extract)[2:ncol(raster.extract)], 
                                                                                                                nchar(names(raster.extract)[2:ncol(raster.extract)]) - 4,
                                                                                                                nchar(names(raster.extract)[2:ncol(raster.extract)]))))
    raster.extract <- raster.extract |> 
      pivot_longer(!ID, names_to = "Date", values_to = vars[v]) #numbers correspond with vector length, if filename changes need to updat
    nc.data <- rbind(nc.data,raster.extract)
    }
  }
  if(v==1){final.extract=nc.data} else{final.extract=full_join(final.extract,nc.data,by=c("ID","Date"))}
}


ids <- unique(final.extract$ID)
for(i in 1:length(ids)){
 Gridmet <- final.extract |> filter(ID == ids[i]) |>
  mutate(PrcpIn = precip/25.4,
         TmaxF = (tmax * (9/5)) - 459.67,
         TminF = (tmin * (9/5)) - 459.67,
         RHmaxPct = rmax,
         RHminPct = rmin,
         TavgF = (TmaxF+TminF)/2,
         Year = year(Date),
         GCM = "gridmet.historical") |> 
  select(Date,GCM,PrcpIn, TmaxF, TminF, RHmaxPct, RHminPct, TavgF, Year)
write.csv(Gridmet, file=paste0(OutDir,ids[i],"_historical.csv"),row.names=FALSE)
}


# Section below grayed out -- need to perform on each DF once read in
# ######################## Functions #############################################
# TFtoC <- function(T){(T-32)/1.8}
# 
# # VP from FAO -  https://www.fao.org/3/x0490e/x0490e07.htm
# # could also use Buck 1981 for 'improved':
# # Buck: VPDsat (mb) = (1.0007 + (3.46 * 10^-6 * P)) * 6.1121 * exp((17.50 * T)/(T+240.87))
# # where T is deg C and P is atm pressure mb. Above for P > 800 (correction is minimal)
# # Zackman re: Ragwala uses: Es = 611.6441 * 10^[(7.591386*T)/(240.7263+T)] where Tavg.
# #   Shelley sent Vaisala- to use Tavg for 611.6441 parameter - for 020 to +50 C.
# # VPsatT = saturation VP @ T deg C [kPa]
# # VPD [kPa]
# VPsatT <- function(T){0.6108 * exp((17.27 * T)/(T + 237.3))}   
# 
# VPD <- function(TminF, TmaxF, RHmin, RHmax){
#   Tmin <- TFtoC(TminF); Tmax <- TFtoC(TmaxF)
#   es <- (VPsatT(Tmin)+VPsatT(Tmax))/2
#   ea <- (VPsatT(Tmin)*RHmax*.01 + VPsatT(Tmax)*RHmin*.01)/2
#   es - ea   }  # end VPD  
# 
# Future_all <- Future_all |> mutate(RCP = stringr::str_sub(GCM,-5,-1),
#                                    VPD = VPD(TminF, TmaxF, RHminPct, RHmaxPct),
#                                    DOY = yday(Date))   # for plotting
# 
# Gridmet <- Gridmet |> mutate(RCP = "Hist",
#                              VPD = VPD(TminF, TmaxF, RHminPct, RHmaxPct),
#                              DOY = yday(Date))   # for plotting
# 
# rm(final.extract,gcm.all,nc.data,raster.extract)
