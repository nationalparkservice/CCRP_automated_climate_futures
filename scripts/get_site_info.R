#####################################################################
###   Pulling local parameters from park files ######
#####################################################################

epsg <- 5070 # North American Albers Equal Area Coni

# shapefiles - can use epsg code or existing object to define projection

nps_centroids <- st_read('./data/general/spatial-data/nps_boundary_centroids/nps_boundary_centroids.shp')
nps_centroids <- st_transform(nps_centroids, st_crs(epsg))
US_States <- st_read('./data/general/spatial-data/State_Shapefile/Contig_US_Albers.shp')
US_States <- st_transform(US_States, st_crs(epsg))

# select park

park <- filter(nps_centroids, UNIT_CODE == SiteID) 

park <- if(nrow(park)>1) {
  park[!grepl("Preserve", park$UNIT_TYPE),]
  } else{park}
#if 2 units and one has "preserve" in name, do not use
s<-US_States %>% filter(STATE_ABBR == park$STATE)

# TWO DIFFERENT OPTIONS FOR CENTROID - use 1st option if running a general RSS and using park centroid. Second option if using specific lat long.
state = s$STATE_NAME
Lat = park$Lat
Lon = park$Lon
region = park$REGION
  
rm(nps_centroids, US_States,s)
