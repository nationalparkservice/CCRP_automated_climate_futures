# Data extraction from Tercek cloud location

# Read CSV files from WB project downloaded from:
  #  https://parkfutures.s3.us-west-2.amazonaws.com/maca-tprh-data/index.html

#################################################
#################   Functions  ##################     
checkFile <- function(fName, fileURL=F){
  tmpFiles <- list.files(here::here(DataDir))
  if(fName %in% tmpFiles){
    print(paste(fName, " File exists. Not downloaded"))
    if("zip" == tolower(str_sub(fName, str_length(fName)-2, str_length(fName))))unzip(paste(DataDir,fName,sep='')) 
    return(1)
  }  # 1 file exists
  # no file so go get it
  print(paste("####  retrieving ", fName, " data file. Might take a while.  ######"))
  options(timeout = 300)   # seconds. How long are you willing to wait?
  getName <- paste("https://parkfutures.s3.us-west-2.amazonaws.com/maca-tprh-data/",fName, sep='')
  
  if(fileURL!=F)getName <- paste(fileURL, fName, sep='')
  download.file(getName, destfile=paste(DataDir,fName,sep=''), mode="wb")
  # unzip if necessary
  if("zip" == tolower(str_sub(fName, str_length(fName)-2, str_length(fName))))unzip(paste(DataDir,fName,sep=''))
}   # end checkFile


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


#################################################################
##############  Create Data Sets - Hist and Proj  ###############

histInFile <-  str_c(SiteID,"_historical.csv", sep='')        # histInFile <- file.choose()    #  browse to path/file
projInFile <- str_c(SiteID,"_future.csv", sep='')
checkFile(histInFile)

Gridmet<- read.csv(here::here(DataDir, histInFile))
names(Gridmet) <- c("Date","GCM","PrcpIn","TmaxF","TminF","RHmaxPct","RHminPct","TavgF")

Gridmet <- Gridmet %>% mutate(Year = year(Date),
                              RCP = "Hist",
                              VPD = VPD(TminF, TmaxF, RHminPct, RHmaxPct),
                              DOY = yday(Date))   # for plotting

checkFile(projInFile)
Future_all <- read.csv(here::here(DataDir, projInFile))
Future_all$Year <- year(Future_all$Date)
Future_all$RCP <- str_sub(Future_all$GCM, str_length(Future_all$GCM)-1, str_length(Future_all$GCM))

names(Future_all) <- c("Date","GCM","PrcpIn","TmaxF","TminF","RHmaxPct","RHminPct","TavgF","Year", "RCP")

Future_all <- Future_all %>% #dplyr::filter(GCM %in% wbGCMs) %>%
  mutate(VPD = VPD(TminF, TmaxF, RHminPct, RHmaxPct),
         DOY = yday(Date))

#### Extract WB data ############
#  1,124,960 obs with all GCMs.   731,224 obs after filter

##############  Initials   ################
WBFileURL <- "https://parkfutures.s3.us-west-2.amazonaws.com/park-centroid-wb/"

hName <- str_c(SiteID, "_water_balance_historical.zip")
checkFile(hName,WBFileURL)  

fName <- str_c(SiteID, "_water_balance_future.zip")
checkFile(fName,WBFileURL)

file.rename(paste0(SiteID,"_water_balance_historical.csv"),paste0(DataDir,SiteID,"_water_balance_historical.csv"))
file.rename(paste0(SiteID,"_water_balance_future.csv"),paste0(DataDir,SiteID,"_water_balance_future.csv"))
file.remove(list.files(path = DataDir, pattern = '.zip', full.names = TRUE))

histWB <- read.csv(paste(DataDir, SiteID,"_water_balance_historical.csv", sep=''))
futWB <- read.csv(paste(DataDir, SiteID, "_water_balance_future.csv", sep=''))


rm(hName, fName)

WBdat <- rbind(histWB, futWB)
rm(histWB, futWB)   

names(WBdat) <- c("Date","GCM","D.in","AET.in","SM.in","Runoff.in","Rain.in",   # Date be consistent with Janelle's code
                  "SWEaccum.in","PET.in")

WBdat <- WBdat %>% mutate(Year=year(Date),
                          Month=month(Date),
                          DOY = yday(Date))  %>%
  mutate(  Seas=case_when
           (3 > Month | Month > 11 ~"Win",
             6 > Month & 2 < Month ~ "Spr",
             9 > Month & 5 < Month ~ "Sum",
             12 > Month & 8 < Month ~ "Fall"))    
