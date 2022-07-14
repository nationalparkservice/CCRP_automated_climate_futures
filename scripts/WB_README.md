
### Instructions for using CCRP's custom WaterBalance package with the Reproducible Climate Futures workflow

The WaterBalance package is an R package for implementing Dave Thoma's water balance spreadsheet model. The package can be found on [GitHub](https://github.com/CCRP-Adaptation/WaterBalance). It is maintained by the NPS Climate Change Response Program. 

The RCF workflow should work seamlessly with the WaterBalance package _once it is set up and if no changes are made to R or WaterBalance package versions_. If you have **recently updated R or the Water Balance package, please follow these instructions:**

* Following major R version updates, the WaterBalance package needs to be rebuilt. To do this, follow instructions in the WaterBalance repo on [GitHub](https://github.com/CCRP-Adaptation/WaterBalance/blob/master/README.md) 

**Using the WaterBalance package with renv**

Step 1. Make sure renv knows where to find local packages
  - Run `r RENV_PATHS_LOCAL = './local-repos'`
  
Step 2. Install Package into environment using tarball
  - Run `r install.packages("./local-repos/WaterBalance_1.1.0.tar.gz")`
  
Step 3. Restart RStudio
  
Step 4. Make sure lockfile is up-to-date, either by pulling the most recent version or by taking a `snapshot()` to record the most up-to-date package versions into the lockfile. *Only take a snapshot if your packages are more current than the versions recorded in the remote repository. In most cases, you should `pull` the most recent version*
  
Step 5. Restore library to package versions in lockfile
  - Run `r renv::restore()`

  
**Using the WaterBalance package with your general R library**

*If you followed the directions for rebuilding the package, the updated package should already exist in your general library*. In this case, all you need to do is tell R not to activate renv (see below). 

Step 1. Open the file `./.Rprofile`

Step 2. Prohibit R from activating the `activate.R` file by commenting out the line `r source("renv/activate.R")`. Restart R with this line commented out, and renv will no longer be activated. _Please do not push your file with this line commented out. Remove the `r #` before pushing any changes!_

The `WaterBalance` package should run from your general library. If it does not, try ```r install.packages("WaterBalance")```. 

*If you are ever unsure whether packages are being pulled from your local `renv` library or your general library, you can always type ```r .libPaths()``` to find out where packages are being run from. *



