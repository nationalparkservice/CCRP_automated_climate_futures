#####################################################################
###   Pulling Site Parameters for D. Thoma Water Balance Model ######
#####################################################################
# Create output directory for maps
figs <- './figures' # for figures
if(dir.exists(figs) == FALSE){
  dir.create(figs)
} 
rm(figs)

maps = './figures/maps'
if(dir.exists(maps) == FALSE){
  dir.create(maps)
}

park_specific = './data/park-specific' # Create folder for park-specific data (i.e. parsed data and output)

if(dir.exists(park_specific) == FALSE){
  dir.create(park_specific)
}

# Set projection to be used for all spatial data:

proj4 <-"+init=epsg:5070" #  North America Albers Equal Area Conic
epsg <- 5070 # North American Albers Equal Area Conic

###  Spatial Data  #####
# This data is available from NPS

# rasters - use proj4 to define projection

maca <- raster('./data/general/spatial-data/Climate_grid/tdn_90d.nc') 
maca <- projectRaster(maca, crs = proj4)
dem <- raster('./data/general/spatial-data/elevation_cropped.tif') 
dem <- projectRaster(dem, crs = proj4)
soil <- raster('./data/general/spatial-data/water_storage.tif') 
soil <- projectRaster(soil, crs= proj4)

# shapefiles - can use epsg code or existing object to define projection

nps_boundary <- st_read('./data/general/spatial-data/nps_boundary/nps_boundary.shp')
nps_boundary <- st_transform(nps_boundary, st_crs(maca))
nps_centroids <- st_read('./data/general/spatial-data/nps_boundary_centroids/nps_boundary_centroids.shp')
nps_centroids <- st_transform(nps_centroids, st_crs(maca))
US_Counties <- st_read('./data/general/spatial-data//US_Counties/tl_2016_us_county.shp')
US_Counties <- st_transform(US_Counties, st_crs(maca))
US_States <- st_read('./data/general/spatial-data/State_Shapefile/Contig_US_Albers.shp')
US_States <- st_transform(US_States, st_crs(maca))

# select park

park <- filter(nps_boundary, UNIT_CODE == SiteID)
State <- filter(US_States, STATE_NAME == state)

# TWO DIFFERENT OPTIONS FOR CENTROID - use 1st option if running a general RSS and using park centroid. Second option if using specific lat long.

centroid <- filter(nps_centroids, UNIT_CODE == SiteID) # use this line if using park centroid

if(exists("MACA_lat") == TRUE){
  centroid <- data.frame(Lat = MACA_lat, Lon = MACA_lon) %>% 
  st_as_sf(coords=c("Lon", "Lat"))
  
  centroid <- st_set_crs(centroid, "+proj=longlat +datum=NAD83 +no_defs")
  centroid <- st_transform(centroid, st_crs(maca)) 
}

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

centroid<- as_Spatial(centroid) # objects must be Spatial (sp) to work with raster package (cannot be sf)
cell <- cellFromXY(maca, centroid) # find grid cell park centroid lies within
maca_cell <- rasterFromCells(maca, cell) # create stand-alone raster for single MACA cell
maca.poly <- rasterToPolygons(maca_cell) # Create MACA polygon - original file in lat/long (note: datum differs from park shapefiles)

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
points <- spsample(maca.poly, n = 10, type = "random")

# plot to check points appear within borders of MACA cell. 

tm_shape(park) + 
  tm_borders() + 
  tm_shape(maca.poly) + 
  tm_borders() + 
  tm_shape(points) + 
  tm_dots()

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
wb_sites # check to be sure values are populated correctly. There should not be NA values. 

write.csv(wb_sites, file = paste('./data/park-specific/', SiteID, " WB site parameters ", Sys.Date(), ".csv", sep = ""), row.names = FALSE)

rm(proj4, epsg, dem, soil, wb_sites,US_States,US_Counties,state_and_park,State,slope,points,park_and_centroid,
   nps_centroids,nps_boundary,latlong,centroid,aspect)
