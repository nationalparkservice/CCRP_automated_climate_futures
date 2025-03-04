#####################################################################
###   Pulling Site Parameters for D. Thoma Water Balance Model ######
#####################################################################

# Set projection to be used for all spatial data:

proj4 <-"+init=epsg:5070" #  North America Albers Equal Area Conic
epsg <- 5070 # North American Albers Equal Area Conic


# Function to set projections
test.reproj.poly <- function(poly, r){
if (st_crs(poly) != crs(r)) {
  poly <- st_transform(poly, st_crs(r))
  }
}

test.reproj.rast <- function(poly, r){
  if (st_crs(poly) != crs(r)) {
    r <- projectRaster(r, crs=st_crs(poly)$wkt)
  }
}



###  Spatial Data  #####
# This data is available from NPS
maca <- raster('./data/general/spatial-data/Climate_grid/tdn_90d.nc') 
maca <- projectRaster(maca, crs = proj4)

dem <- raster('./data/general/spatial-data/elevation_cropped.tif') 
dem <- projectRaster(dem, crs = proj4)

soil <- raster('./data/general/spatial-data/water_storage.tif') 
soil <- projectRaster(soil, crs= proj4)

land <- raster("~/GIS/NLCD_2023/ncld.land.tif")

# shapefiles - can use epsg code or existing object to define projection
nps_boundary <- st_read("~/GIS/CONUS Refuge Boundary/CONUS Refuge Boundary 3_18_2024/CONUS_NWR_Approved.shp")
nps_boundary <- test.reproj.poly(nps_boundary, maca)

nps_centroids <- st_read("~/GIS/CONUS NWRS Centroids/CONUS NWRS Centroids/CONUS_refuge_centroids.shp")
nps_centroids <- test.reproj.poly(nps_centroids, maca)

US_States <- st_read('./data/general/spatial-data/State_Shapefile/Contig_US_Albers.shp')
US_States <- test.reproj.poly(US_States, maca)

# select park
nps_boundary$UNIT_CODE <- nps_boundary$LIT
park <- filter(nps_boundary, UNIT_CODE == SiteID)

State <- filter(US_States, STATE_ABBR == state)
s <- test.reproj.poly(State,land)
land <- crop(land,s); rm(s)
land <- projectRaster(land, crs= proj4)
land <- test.reproj.rast(park, land) #this will take a long time
land2 <- rasterToPolygons(mask(land, park[1]))

# TWO DIFFERENT OPTIONS FOR CENTROID - use 1st option if running a general RSS and using park centroid. Second option if using specific lat long.
nps_centroids$UNIT_CODE <- nps_centroids$LIT
centroid <- filter(nps_centroids, UNIT_CODE == SiteID) 
# centroid <- filter(nps_centroids, UNIT_CODE == SiteID) # use this line if using park centroid


# Check that spatial data looks OK so far. Precise projection doesn't matter at this point but should be close. 
# You should see the park outline by itself, and also where it lies within the state.

state_and_park <- tm_shape(State) +
  tm_borders() + 
  tm_fill(col = "lightgrey") +
  tm_shape(park) +
  tm_borders() + 
  tm_fill(col = "green")

park_and_centroid <- tm_shape(park) + 
  tm_borders() +
  tm_fill(col = "lightgreen") + 
  tm_shape(centroid) + 
  tm_dots(size = 1, shape = 3)

tmap_arrange(state_and_park, park_and_centroid)

# Obtain MACA grid outline (not information within)
park <- as_Spatial(park)
centroid<- as_Spatial(centroid) # objects must be Spatial (sp) to work with raster package (cannot be sf)
cell <- cellFromXY(maca, centroid) # find grid cell park centroid lies within
maca_cell <- rasterFromCells(maca, cell) # create stand-alone raster for single MACA cell
maca.poly <- rasterToPolygons(maca_cell) # Create MACA polygon - original file in lat/long (note: datum differs from park shapefiles)

intersect.shp <- st_intersection(st_as_sf(maca.poly),st_as_sf(park[1])) #intersect maca cell to park boundary
intersect.shp <- st_intersection(intersect.shp,st_as_sf(land2))
sampling.boundary <- as_Spatial((intersect.shp))


# Plot to see that MACA cell is visible and appropriately located within park

tm_shape(park) + 
  tm_borders() + 
  tm_shape(centroid) + 
  tm_dots() + 
  tm_shape(maca.poly) + 
  tm_borders()


#####   SLOPE, ASPECT AND RANDOM POINTS   ##########################################################################################

# Create slope and aspect rasters

slope <- terrain(dem, opt = "slope", unit = "degrees", neighbors = 4) # 4 is better for "smooth" surfaces; 8 is better for rough. See https://www.rdocumentation.org/packages/raster/versions/3.1-5/topics/terrain
aspect <- terrain(dem, opt = "aspect", unit = "degrees")

# get 10 random points from soil raster and create SpatialPoints object
points <- spsample(sampling.boundary, n = 10, type = "random")

# plot to check points appear within borders of MACA cell. 
png(paste0(OutDir,"WBpoints.png"), width=4, height=4, units="in", res=300,)
tm_shape(park) + 
  tm_borders() + 
  tm_shape(maca.poly) + 
  # tm_shape(s) +
  tm_borders() + 
  tm_shape(points) + 
  tm_dots()
dev.off()


####    EXTRACT DATA FROM POINTS  ######################################################################################################

# reproject points to lat/long so can eventually add to .csv

latlong <- st_as_sf(points) # convert to sf object 
latlong <- st_transform(latlong, crs = 4326) # project to lat/long

wb_sites <- as.data.frame(st_coordinates(latlong)) # begin new dataframe for wb_sites

wb_sites[,3] <- raster::extract(dem, points)
wb_sites[,4] <- raster::extract(aspect, points)
wb_sites[,5] <- raster::extract(slope, points)
wb_sites[,6] <- raster::extract(soil, points)
wb_sites[,7] <- seq.int(nrow(wb_sites))
wb_sites[,8] <- 5 # default value for wind
wb_sites[,9] <- 0 # default value for snowpack
wb_sites[,10] <- 0 # default value for Soil.Init
wb_sites[,11] <- 1 # default value for shade coefficient

wb_sites <- select(wb_sites, 7,2,1,3:6, 8:11) # reorder columns
colnames(wb_sites) <- c("WB_site", "Lat", "Lon", "Elev", "Aspect", "Slope", "SWC.Max", "Wind", "Snowpack", "Soil.Init", "Shade.Coeff")

wb_sites$SWC.Max = wb_sites$SWC.Max*10 # convert units for Soil Water-holding capacity
wb_sites$SWC.Max[is.na(wb_sites$SWC.Max)] <- mean(wb_sites$SWC.Max,na.rm=TRUE)
wb_sites$Aspect[(which(wb_sites$Elev==0)&is.na(wb_sites$Aspect))] <- 0
wb_sites$Slope[(which(wb_sites$Elev==0)&is.na(wb_sites$Slope))] <- 0
wb_sites # check to be sure values are populated correctly. There should not be NA values. 

write.csv(wb_sites, file = paste0(OutDir,"WB_site_parameters.new.csv"), row.names = FALSE)
rm(proj4, epsg, dem, soil,US_States,US_Counties,state_and_park,State,slope,points,park_and_centroid,
   nps_centroids,nps_boundary,latlong,centroid,aspect)
