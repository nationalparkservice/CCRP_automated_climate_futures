#####
# Note to ACR - Need to rename var in bcc-csm1-1-m and add it back into GCMs list



#######################################################################################
# Script for extracting historic and future projection data from daily CONUS MACAv2 dataset
# Inputs: lat/lon coordinates, directory of .ncdf4 MACA data files, selected GCMs, and historical/future start and end years
# Outputs: List of data frames containing all daily values for each scenario and variable, with data columns separated by GCM 
#   Converts mm to inches and Kelvins to degrees F
#######################################################################################

#v01 - Running - Possible embellishment to include multiple grid cell parsing

library(ncdf4)
library(reshape2)
library(dplyr)
library(lubridate)

############################# User-defined initials #################################
rm(list=ls())
# ####Location for data extraction
DataDir <- "C:/Users/arunyon/3D Objects/Local-files/RCF_Testing/MACA-test/"


site.data <- read.csv("C:/Users/arunyon/Downloads/Sites.csv")
SiteID = site.data$SiteID[5]
Lat = site.data$Lat[5]
Lon = site.data$Lon[5]
# SiteID = "Buckinghorse"
# #Cell resolution is 0.041666 degrees, reference coordinates are cell centers
# Lat = 47.7085955		
# Lon = -123.4574713
cLon = Lon + 360 #Adjusts negative lon. values 

###Parameters for extracting data from NetCDF files
#Specify top-level directory where MACA data is stored (all scenarios and variables)
MACADir = "D:/ClimateData/MACAv2Metdata" #no '/' at the end

# OutDir = paste0('./PARK/',SiteID,"/") # for .csv's
# if(dir.exists(OutDir) == FALSE){
#   dir.create(OutDir)
# }
# 
# DataDir = paste0(OutDir,'input-data/') # for .csv's
# if(dir.exists(DataDir) == FALSE){
#   dir.create(DataDir)
# }

#Variable and scenario names corresponding to MACA data directory structure
vars = c("pr", "tasmax", "tasmin","rhsmax","rhsmin")
scens = c("rcp45", "rcp85")

#Variable names for output tables
VarNames = c("PrecipCustom", "TmaxCustom", "TminCustom","RHmaxCustom","RHminCustom")

# GCMs to be extracted
GCMs = c('bcc-csm1-1','bcc-csm1-1-m','BNU-ESM','CanESM2','CCSM4','CNRM-CM5','CSIRO-Mk3-6-0',
         'GFDL-ESM2G','GFDL-ESM2M','HadGEM2-CC365','HadGEM2-ES365',
         'inmcm4','IPSL-CM5A-MR','IPSL-CM5A-LR','IPSL-CM5B-LR',
         'MIROC5','MIROC-ESM','MIROC-ESM-CHEM','MRI-CGCM3','NorESM1-M')

#Date ranges to be extracted
Future_StartYear = 2024   #2006-2099
Future_EndYear = 2098   #2006-2099


######################### End user-defined initials ##############################

######################## Functions #############################################
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


################### Extract daily data series from NetCDF files ##################

#Date ranges
Future_StartDate <- as.Date(paste(Future_StartYear, "01", "01", sep="-"))
Future_EndDate <- as.Date(paste(Future_EndYear, "01", "01", sep="-"))
Future_DateRange <- seq.Date(Future_StartDate, Future_EndDate, by="1 day")

start.time <- Sys.time()
#Will store data frames of time series extracted for each scenario and variable. 
#.ncdf files separated by time period will be combined into a single series for each scenario, variable, and GCM.
DataExtraction <- list(0)
j <-1 #index for output list

for(scen in scens){
  scenDir <- paste(MACADir, scen, sep="/")
  scenIndex <- which(scens == scen)
    StartYr <- Future_StartYear
    EndYr <- Future_EndYear
    StartDate <- Future_StartDate
    EndDate <- Future_EndDate
    DateRange <- Future_DateRange
  
  for(var in vars){
    writeLines("")
    print(paste("Extracting", scen, var, "data"))
    writeLines("")
    ScenVarDF <- data.frame(Dates=DateRange)
    varDir <- paste(scenDir, var, sep="/")
    nc.files <- c()
    for(GCM in GCMs){
      GCM.sub <- list.files(varDir, pattern = paste("_", GCM, "_", sep=""))
      GCM.sub <- GCM.sub[grepl("\\.nc$", GCM.sub)]
      for(file in GCM.sub){
        year1 <- as.numeric(strsplit(file, "_")[[1]][6])
        year2 <- as.numeric(strsplit(file, "_")[[1]][7])
        if((year1 >= StartYr & year1 <= EndYr) | (year2 >= StartYr & year2 <= EndYr)){
          nc.files <- c(nc.files, file)
        }
      }
    }
    
    #Get information from first file, to be used for all extractions
    nc1 <- nc_open(paste(varDir, nc.files[1], sep="/"))
    varName <- names(nc1$var)
    varUnits <- ncatt_get(nc1, varName, "units")$value
    All_lat <- data.frame(nc1$dim$lat$vals)
    All_lon <- data.frame(nc1$dim$lon$vals)
    Lat_index = as.numeric(which.min(abs(All_lat$nc1.dim.lat.vals - Lat)))
    Lon_index = as.numeric(which.min(abs(All_lon$nc1.dim.lon.vals - cLon)))
    GCM <- strsplit(nc.files[1], "_")[[1]][3]
    
    #Loop through data and separate by GCM 
    Dates <- as.Date(nc1$dim$time$vals, origin = "1900-01-01")
    Extr <- ncvar_get(nc1, varName, c(Lon_index, Lat_index, 1), count=c(1,1,-1))   
    GCM_data <- data.frame(Dates, Extr)
    nc_close(nc1)
    for(i in 2:length(nc.files)){
      GCM.file <- strsplit(nc.files[i], "_")[[1]][3]
      #Extract data
      nc <- nc_open(paste(varDir, nc.files[i], sep="/"))
      varName <- names(nc$var)
      Dates <- as.Date(nc$dim$time$vals, origin="1900-01-01")
      Extr <- ncvar_get(nc, varName, c(Lon_index, Lat_index, 1), count=c(1,1,-1))   
      Extr_DF <- data.frame(Dates, Extr)
      #Add each data series to separate GCM column within scen/var data frame
      if(GCM.file == GCM){
        ##Extract data array and append to data frame
        GCM_data <- rbind(GCM_data, Extr_DF)       
      }
      else{
        print(paste(GCM, "data extracted"))
        #Append extracted data series to data frame
        ScenVarDF <- merge(ScenVarDF, GCM_data, by="Dates")
        if(varUnits == "mm"){
          ScenVarDF$Extr <- ScenVarDF$Extr/25.4
        }
        else if(varUnits == "K"){
          ScenVarDF$Extr <- (ScenVarDF$Extr * (9/5)) - 459.67
        }
        #Rename last column
        colnames(ScenVarDF)[ncol(ScenVarDF)] <- GCM
        #Begin new data series 
        GCM_data <- data.frame(Dates, Extr)
        GCM <- GCM.file
      }
      nc_close(nc)
    }
    ScenVarDF <- merge(ScenVarDF, GCM_data, by="Dates")
    print(paste(GCM, "data extracted"))
    if(varUnits == "mm"){
      ScenVarDF$Extr <- ScenVarDF$Extr/25.4
      Units <- "In"
    }
    else if(varUnits == "K"){
      ScenVarDF$Extr <- (ScenVarDF$Extr * (9/5)) - 459.67
      Units <- "degF"
    }
    else{
      ScenVarDF$Extr <- ScenVarDF$Extr
      Units <- varUnits
    }
    colnames(ScenVarDF)[ncol(ScenVarDF)] <- GCM
    
    ScenVarName <- paste(scen, var, sep=".")
    DataExtraction[[j]] <- ScenVarDF
    names(DataExtraction)[j] <- ScenVarName
    attr(DataExtraction[[j]], "units") <- Units
    j <- j+1
    rm(ScenVarDF, All_lat, All_lon)
  }
} 

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

### End data extraction ###

##################################################################################
#####Baseline_all - Daily time series for all GCMs for the historical period

### Summarize daily data ###

#Future
RCP45_Data <- DataExtraction[grep("rcp45", names(DataExtraction))]
RCP85_Data <- DataExtraction[grep("rcp85", names(DataExtraction))]

df1 <- RCP45_Data[[1]]
df2 <- RCP85_Data[[1]]
df1.melt <- melt(df1, id="Dates")
df1.melt$variable <- paste(df1.melt$variable, "rcp45", sep=".")
df2.melt <- melt(df2, id="Dates")
df2.melt$variable <- paste(df2.melt$variable, "rcp85", sep=".")
Future_all <- rbind(df1.melt, df2.melt)
colnames(Future_all) <- c("Date", "GCM", VarNames[1])

for(i in 2:length(vars)){
  df1 <- RCP45_Data[[i]]
  df2 <- RCP85_Data[[i]]
  df1.melt <- melt(df1, id="Dates")
  df1.melt$variable <- paste(df1.melt$variable, "rcp45", sep=".")
  df2.melt <- melt(df2, id="Dates")
  df2.melt$variable <- paste(df2.melt$variable, "rcp85", sep=".")
  df <- rbind(df1.melt, df2.melt)
  colnames(df) <- c("Date", "GCM", VarNames[i])
  Future_all <- merge(Future_all, df, by=c("Date", "GCM"),all=T)
}

####
# Reformat DF and save
head(Future_all)

Future_all <- Future_all |>  rename_with(~c("Date","GCM","PrcpIn","TmaxF","TminF","RHmaxPct","RHminPct")) |> 
  mutate(TavgF = (TmaxF+TminF)/2,
         Year = year(Date)) 
write.csv(Future_all, file=paste0(DataDir,SiteID,"_future.csv"),row.names=FALSE)

Future_all <- Future_all |> mutate(RCP = stringr::str_sub(GCM,-5,-1),
          VPD = VPD(TminF, TmaxF, RHminPct, RHmaxPct),
          DOY = yday(Date))   # for plotting

rm(DataExtraction, RCP45_Data, RCP85_Data)

#### GRIDMET data
GridDir <- "D:/ClimateData/gridmet"
vars = c("precip", "tmax", "tmin", "rmax","rmin") #folder names

for(var in vars){
  writeLines("")
  print(paste("Extracting", var, "data"))
  writeLines("")
  varDir <- paste(GridDir, var, sep="/")
  nc.files <- c()
  files <- list.files(varDir)
  d=setNames(data.frame(matrix(ncol=2,nrow=0)),c("Dates","Extr"))
  
  for(file in files){
    if(file==files[1]){
      Data1=d
    }
    nc<-nc_open(paste(varDir,file,sep="/"))
    varName <- names(nc$var)
    varUnits <- ncatt_get(nc, varName, "units")$value
    All_lat <- data.frame(nc$dim$lat$vals)
    All_lon <- data.frame(nc$dim$lon$vals)
    Lat_index = as.numeric(which.min(abs(All_lat$nc.dim.lat.vals - Lat)))
    Lon_index = as.numeric(which.min(abs(All_lon$nc.dim.lon.vals - Lon)))
    
    #Loop through data and separate by GCM 
    Date <- as.Date(nc$dim$day$vals, origin = "1900-01-01")
    Extr <- ncvar_get(nc, varName, c(Lon_index, Lat_index, 1), count=c(1,1,-1))
    if(varUnits == "mm"){
      Extr <- Extr/25.4
    }
    else if(varUnits == "K"){
      Extr <- (Extr * (9/5)) - 459.67
    }
    Data <- data.frame(Date, Extr)
    nc_close(nc)
    Data1<-rbind(Data1,Data)
  }
  colnames(Data1)[2]<-var
  Data2<-Data1[order(Date),]
  if (var == vars[1]){Gridmet<-Data1} else {Gridmet<-cbind(Gridmet,Data1[2])}
  assign(var,Data1)
}

head(Gridmet)

Gridmet <- Gridmet |> mutate(Date = Date,
                   GCM="gridmet.historical",
                   PrcpIn = precip,
                   TmaxF = tmax,
                   TminF = tmin,
                   RHmaxPct = rmax,
                   RHminPct = rmin,
                   TavgF = (TmaxF+TminF)/2,
                   Year = format(Date,"%Y")) |> 
  select(c("Date","GCM","PrcpIn","TmaxF","TminF","RHmaxPct","RHminPct","TavgF","Year")) 
write.csv(Gridmet, file=paste0(DataDir,SiteID,"_historical.csv"),row.names=FALSE)

Gridmet <- Gridmet |> mutate(RCP = "Hist",
                                   VPD = VPD(TminF, TmaxF, RHminPct, RHmaxPct),
                                   DOY = yday(Date))   # for plotting

rm(All_lat,All_lon, d,Data,Data1,Data2, nc, precip, rmax,rmin,tmax,tmin,Extr,file,files,Future_DateRange,Future_EndDate,
   Future_EndYear,Future_StartDate,Future_StartYear,Lat_index,Lon_index,varName)
