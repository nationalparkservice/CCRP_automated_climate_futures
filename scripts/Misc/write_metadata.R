##################################################
##    METADATA    ################################
##################################################
DataFile <- list.files(path = DataDir, pattern = 'Final_Environment.RData', full.names = TRUE) # Environment needs to be added if not parsing MACA data
load(DataFile)

my_session_info <- devtools::session_info()
SI <- sessionInfo()

# This script prints analysis metadata into the sessionInfo.txt file
sink(paste0(OutDir,"sessionInfo.txt")) # Create sessionInfo text file

# sessionInfo() # add information on package and R versions
cat("\n")

cat(paste("Climate futures for",SiteID))
cat("\n")
cat(paste("Lat:", Lat))
cat("\n")
cat(paste("Lon:", Lon))
cat("\n")
cat(paste("Base period:",BasePeriod,"using Gridmet as reference data"))
cat("\n")
cat(paste("Future period:", Range,"-yrs, centered on", Yr, "(",Yr-Range/2,"-",Yr+Range/2,")"))
cat("\n")

# Input files

cat("\n")
if(centroids_csv == "Y"){
  cat("Gridmet, MACA, and water balance data downloaded from Tercek .csv files")
} else {
    cat("MACA and gridmet data downloaded from lat/lon. Water balance calculated from R package.")
}
cat("\n")
inputs <- list.files(path = DataDir) # RData files created from parsed data

cat("input data files:")

for(i in 1:length(inputs)){
  cat(paste0(inputs[i], "; "))
}
cat("\n")

#inputs[1], inputs[2], inputs[3], inputs[4], inputs[5], sep = " "))

# Water Balance model
# NOTE: Information on Water Balance package version is included in sessionInfo() just like any other package from CRAN (or elsewhere)

cat("\n")
cat("WATER BALANCE MODEL INFORMATION")
cat("\n")
cat(paste("Models used in Water Balance analysis:", WB_GCMs[1], WB_GCMs[2])) 
cat("\n")

# Drought analysis

cat("\n")
cat("DROUGHT ANALYSIS")
cat("\n")
cat(paste("SPEI aggregation period (in months):", SPEI_per))
cat("\n")
cat(paste("SPEI truncation value:", truncation))
cat("\n")
cat(paste("SPEI baseline starts in", SPEI_start), "and ends in", SPEI_end)
cat("\n")

# # Bias correction text
# # NOTE: This will probably be moved to a Methods document at some point
# 
# cat("\n")
# cat("BIAS CORRECTION METHODS")
# cat("\n")
# cat(paste("Long-term timeseries plots use PRISM historical data - bias-corrected against gridMET from", BC.min, "to", BC.max, "using the delta method. Projections use MACA data."))
cat("\n")
cat(paste("Program completed at ",Sys.time()))
cat("\n")
cat("R SESSION INFORMATION")
cat("\n")

cat("\n")
cat(R.version.string)
print(my_session_info)
cat("\n")
cat(print(Sys.info()))
cat("\n")
sink()

# # copy to local folder and delete
# 
# txt <- list.files(project_root_dir, pattern = '.txt')
# file.copy(file.path(project_root_dir, txt), local_rss_dir) # If this prints 'FALSE', you may have an existing 'sessionInfo.txt' file in your directory. Remove and re-run. 
# 

unlink("sessionInfo.txt")
