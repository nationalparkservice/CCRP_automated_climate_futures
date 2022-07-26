#####################################################################
###   Pulling local parameters from park files ######
#####################################################################

epsg <- 5070 # North American Albers Equal Area Coni

# shapefiles - can use epsg code or existing object to define projection

nps_centroids <- st_read('./data/general/spatial-data/nps_boundary_centroids/nps_boundary_centroids.shp')
nps_centroids <- st_transform(nps_centroids, st_crs(epsg))

# select park

park <- filter(nps_centroids, UNIT_CODE == SiteID)


# TWO DIFFERENT OPTIONS FOR CENTROID - use 1st option if running a general RSS and using park centroid. Second option if using specific lat long.
Lat = park$Lat
Lon = park$Lon
region = park$REGION
  
rm(nps_centroids)
