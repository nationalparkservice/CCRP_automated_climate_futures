rm(list=ls())

library(devtools)

#Install WaterBalance package from Github repository
install_github("CCRP-Adaptation/WaterBalance", subdir="WaterBalance")

#Or, install from local folder
setwd("~/R/win-library/3.5/") #Directory containing package folder
install("WaterBalance")

library("WaterBalance")

##test