rm(list = ls())
setwd("Z:\\Kimiya\\annual mean PM Heresh")

library(geosphere)
library(gstat)
library(sp)
library(raster)
library(leaflet)
library(tidyverse)
library(mapview)
library(osmdata)
library(sf)

DataT <- readRDS("aqdh_pm25component_no3_2019_urban.rds") 

##### Address 1 #####
# Rename the columns in your Data
Data <- DataT %>%
  rename(latitude = lat, longitude = lon, Value = names(DataT)[3])


# Reference point (latitude and longitude)
address <- "1155 Barroilhet Dr. Hillsborough CA 94010"
ref_latitude <- 37.56566528330899
ref_longitude <- -122.36169540317907


# Calculate distances from the reference location to all points
Data$distance <- distHaversine(c(ref_longitude, ref_latitude), cbind(Data$longitude, Data$latitude))

# Convert distance from meters to kilometers
Data$distance_km <- Data$distance / 1000

# Filter points within 2.5 km
data <- Data[Data$distance_km <= 2.5, ]

# Convert to SpatialPointsDataFrame
data <- as.data.frame(data)
coordinates(data) <- ~longitude+latitude

# Define the bounding box for the grid
grid <- expand.grid(
  longitude = seq(min(data$longitude), max(data$longitude), by = 0.0001),
  latitude = seq(min(data$latitude), max(data$latitude), by = 0.0001)
)

# Convert the grid to a SpatialPixelsDataFrame
coordinates(grid) <- ~longitude+latitude
gridded(grid) <- TRUE

# Perform IDW interpolation
idw_result <- idw(formula = Value ~ 1, locations = data, newdata = grid)

# Convert the result to a raster
raster_surface <- raster(idw_result)

# Set the CRS of the raster to WGS84
crs(raster_surface) <- CRS("+proj=longlat +datum=WGS84")

# Convert raster to data frame for ggplot
raster_df <- as.data.frame(raster_surface, xy = TRUE)

# Get the bounding box of the raster as a spatial object
raster_bbox <- st_as_sfc(st_bbox(raster(raster_surface)))

# Load your shapefile (in this case, street data from OpenStreetMap)
Bounds <- c(min(data$longitude), min(data$latitude), max(data$longitude), max(data$latitude))
street_data <- opq(bbox = Bounds) %>%
  add_osm_feature(key = "highway") %>%
  osmdata_sf()

# Clean up the column names
colnames(street_data$osm_lines) <- gsub("[:]", "_", colnames(street_data$osm_lines))

# Select necessary columns and clip shapefile to raster bounding box
shapefile <- street_data$osm_lines %>% select(osm_id, name)
shapefile_clipped <- st_intersection(shapefile, raster_bbox)

# Plot the raster and clipped shapefile together using ggplot
ggplot() +
  geom_raster(data = raster_df, aes(x = x, y = y, fill = var1.pred)) +
  scale_fill_viridis_c() +  # Color scale for the raster values
  
  # Plot the clipped shapefile
  geom_sf(data = shapefile_clipped, fill = NA, color = "grey70", size = 0.1, stroke = 0.1) +
  scale_fill_gradient(low = "yellow1", high = "red2") + 
  coord_sf() +  # Ensure correct spatial alignment
  labs(title = "", fill = "Nitrate", x = "Longitude", y = "Latitude") +
  theme_minimal() +
  theme(legend.position = "right")+
  geom_point(aes(x = ref_longitude, y = ref_latitude), 
             shape = 25, color = "black", fill = "black", size = 2) +
  geom_text(aes(x = ref_longitude, y = ref_latitude, label = address), 
            vjust = -1, hjust = 0.5, size = 2, color = "black")

# Save the plot as a PDF with the clipped shapefile and raster
ggsave("new address/Map NO3 1155 Barroilhet.pdf", width = 10, height = 8, dpi = 300)
names(raster_df) <- c("Longitude", "Latitude", "NO3")
write.csv(raster_df, "new address/Data NO3 1155 Barroilhet.csv", row.names = F)


##### Address 2 #####
# Rename the columns in your Data
Data <- DataT %>%
  rename(latitude = lat, longitude = lon, Value = names(DataT)[3])


# Reference point (latitude and longitude)
address <- "1054 Mission Rd Pebble Beach CA 93953"
ref_latitude <- 36.60272123236349
ref_longitude <- -121.94345601670643

# Calculate distances from the reference location to all points
Data$distance <- distHaversine(c(ref_longitude, ref_latitude), cbind(Data$longitude, Data$latitude))

# Convert distance from meters to kilometers
Data$distance_km <- Data$distance / 1000

# Filter points within 2.5 km
data <- Data[Data$distance_km <= 2.5, ]

# Convert to SpatialPointsDataFrame
data <- as.data.frame(data)
coordinates(data) <- ~longitude+latitude

# Define the bounding box for the grid
grid <- expand.grid(
  longitude = seq(min(data$longitude), max(data$longitude), by = 0.0001),
  latitude = seq(min(data$latitude), max(data$latitude), by = 0.0001)
)

# Convert the grid to a SpatialPixelsDataFrame
coordinates(grid) <- ~longitude+latitude
gridded(grid) <- TRUE

# Perform IDW interpolation
idw_result <- idw(formula = Value ~ 1, locations = data, newdata = grid)

# Convert the result to a raster
raster_surface <- raster(idw_result)

# Set the CRS of the raster to WGS84
crs(raster_surface) <- CRS("+proj=longlat +datum=WGS84")

# Convert raster to data frame for ggplot
raster_df <- as.data.frame(raster_surface, xy = TRUE)

# Get the bounding box of the raster as a spatial object
raster_bbox <- st_as_sfc(st_bbox(raster(raster_surface)))

# Load your shapefile (in this case, street data from OpenStreetMap)
Bounds <- c(min(data$longitude), min(data$latitude), max(data$longitude), max(data$latitude))
street_data <- opq(bbox = Bounds) %>%
  add_osm_feature(key = "highway") %>%
  osmdata_sf()

# Clean up the column names
colnames(street_data$osm_lines) <- gsub("[:]", "_", colnames(street_data$osm_lines))

# Select necessary columns and clip shapefile to raster bounding box
shapefile <- street_data$osm_lines %>% select(osm_id, name)
shapefile_clipped <- st_intersection(shapefile, raster_bbox)

# Plot the raster and clipped shapefile together using ggplot
ggplot() +
  geom_raster(data = raster_df, aes(x = x, y = y, fill = var1.pred)) +
  scale_fill_viridis_c() +  # Color scale for the raster values
  
  # Plot the clipped shapefile
  geom_sf(data = shapefile_clipped, fill = NA, color = "grey70", size = 0.1, stroke = 0.1) +
  scale_fill_gradient(low = "yellow1", high = "red2") + 
  coord_sf() +  # Ensure correct spatial alignment
  labs(title = "", fill = "Ammonium", x = "Longitude", y = "Latitude") +
  theme_minimal() +
  theme(legend.position = "right")+
  geom_point(aes(x = ref_longitude, y = ref_latitude), 
             shape = 25, color = "black", fill = "black", size = 2) +
  geom_text(aes(x = ref_longitude, y = ref_latitude, label = address), 
            vjust = -1, hjust = 0.5, size = 2, color = "black")

# Save the plot as a PDF with the clipped shapefile and raster
ggsave("new address/Map NO3 1054 Mission.pdf", width = 10, height = 8, dpi = 300)
names(raster_df) <- c("Longitude", "Latitude", "NO3")
write.csv(raster_df, "new address/Data NO3 1054 Mission.csv", row.names = F)
