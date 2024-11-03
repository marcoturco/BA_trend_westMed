# Clear the workspace and graphics, prepare for analysis
rm(list = ls()) # Remove all objects from the current workspace
graphics.off() # Close all open graphics devices
gc() # Perform garbage collection to free up memory

# Working directory 
# setwd("C:/Users/andri/Desktop/tendencias/trends/datos/")
setwd("/Users/marco/Documents/dati/obs/trend_westmed/data/")

library(ncdf4)
library(sf)
library(fields)

data(wrld_simpl)


# Function that assigns NA values to gridpoints out of the domain of interest
mask_domain <- function(field, inout) {
  d <- dim(field)
  nlon <- d[1]
  nlat <- d[2]
  for (i in 1:nlon) {
    for (j in 1:nlat) {
      field[i, j, ] <- field[i, j, ] * inout[i, j]
    }
  }
  return(field)
}

file_shp <-  file.path("domain.shp")
shp <- st_read(file_shp)
CRS.new <- "+proj=longlat +datum=WGS84 +no_defs"
shp <- st_transform(shp, CRS.new)
shp=shp[,1]
shp <- st_union(shp)
plot(shp)
# Get the bounding box of the shapefile
bbox <- st_bbox(shp)

# Extract individual limits
lon_min <- bbox["xmin"]
lon_max <- bbox["xmax"]
lat_min <- bbox["ymin"]
lat_max <- bbox["ymax"]

# # Dataset: ONFIRE
# # --------------------
# 
# # Load main data
load("BA_EUROPE_v1.RData")  # Burned area
load("lon_BA_EUROPE_v1.RData")  # Longitude
load("lat_BA_EUROPE_v1.RData")  # Latitude
dim(BA)
image.plot(lon,lat,apply(BA,c(1,2),mean,na.rm=T))
# Filter lon and lat values within the limits of the shapefile
lon_within_shp <- lon[lon >= lon_min & lon <= lon_max]
lat_within_shp <- lat[lat >= lat_min & lat <= lat_max]
lon_indices <- which(lon >= lon_min & lon <= lon_max)
lat_indices <- which(lat >= lat_min & lat <= lat_max)

points <- expand.grid(lon_within_shp, lat_within_shp)
pts <- st_as_sf(points, coords = c("Var1", "Var2"), crs = CRS.new)

# Check which points fall within the Murcia shapefile
inout <- st_intersects(pts, shp, sparse = FALSE)
if (is.null(inout)) {
  print("No intersection found with the shapefile!")
  return(NULL)
}

# Reshape inout to match obs dimensions
inout[inout == FALSE] <- NA
inout_matrix <- matrix(as.numeric(inout), nrow = length(lon_within_shp), ncol = length(lat_within_shp))  # Reshape

# Plot the FWI mean over time
image.plot(lon_within_shp, lat_within_shp, inout_matrix, main = "In/Out Mask", xlab = "Longitude", ylab = "Latitude")
plot(st_geometry(shp), add = TRUE, border = 'red')  # Add Murcia region as an overlay

BA_within_shp <- BA[lon_indices, lat_indices, ]
# dim(inout_matrix)
# dim(BA_within_shp)
# Apply the domain to the data 
BA_ONFIRE <- mask_domain(BA_within_shp, inout_matrix) 


# # Satellite Dataset at 1º global scale
# # --------------------

# Load longitude and latitude
fname <- file.path('land_sea_mask_1degree.nc4')
obs.nc <- nc_open(fname)
lon <- obs.nc$dim$lon$vals
lat <- obs.nc$dim$lat$vals

# Filter lon and lat values within the limits of the shapefile
lon_within_shp <- lon[lon >= lon_min & lon <= lon_max]
lat_within_shp <- lat[lat >= lat_min & lat <= lat_max]
lon_indices <- which(lon >= lon_min & lon <= lon_max)
lat_indices <- which(lat >= lat_min & lat <= lat_max)


points <- expand.grid(lon_within_shp, lat_within_shp)
pts <- st_as_sf(points, coords = c("Var1", "Var2"), crs = CRS.new)

# Check which points fall within the Murcia shapefile
inout <- st_intersects(pts, shp, sparse = FALSE)
if (is.null(inout)) {
  print("No intersection found with the shapefile!")
  return(NULL)
}

# Reshape inout to match obs dimensions
inout[inout == FALSE] <- NA
inout_matrix <- matrix(as.numeric(inout), nrow = length(lon_within_shp), ncol = length(lat_within_shp))  # Reshape

# Plot the FWI mean over time
image.plot(lon_within_shp, lat_within_shp, inout_matrix, main = "In/Out Mask", xlab = "Longitude", ylab = "Latitude")
plot(st_geometry(shp), add = TRUE, border = 'red')  # Add Murcia region as an overlay



# 

# Load the specific dataset (FIRECCI51)
load("BA_FIRECCI51_200101_202012_1.RData")
# Extract the BA values within these limits
BA_within_shp <- BA[lon_indices, lat_indices, ]
# dim(inout_matrix)
# dim(BA_within_shp)
# Apply the domain to the data 
BA_FIRECCI51 <- mask_domain(BA_within_shp, inout_matrix) 
image.plot(lon_within_shp, lat_within_shp, apply(log(BA_FIRECCI51+1),c(1,2),sd,na.rm=T), main = "In/Out Mask", xlab = "Longitude", ylab = "Latitude")
plot(st_geometry(shp), add = TRUE, border = 'red')  # Add Murcia region as an overlay

load("BA_FIRECCI51_200101_202012_nat_1.RData")
BA_within_shp <- BA[lon_indices, lat_indices, ]
BA_FIRECCI51_nat <- mask_domain(BA_within_shp, inout_matrix) 
image.plot(lon_within_shp, lat_within_shp, apply(log(BA_FIRECCI51_nat+1),c(1,2),sd,na.rm=T), main = "In/Out Mask", xlab = "Longitude", ylab = "Latitude")
plot(st_geometry(shp), add = TRUE, border = 'red')  # Add Murcia region as an overlay

load("BA_gfed5_1997101_202112_10.RData")
BA_within_shp <- BA[lon_indices, lat_indices, ]
BA_GFED5 <- mask_domain(BA_within_shp, inout_matrix) 
image.plot(lon_within_shp, lat_within_shp, apply(log(BA_GFED5+1),c(1,2),sd,na.rm=T), main = "In/Out Mask", xlab = "Longitude", ylab = "Latitude")
plot(st_geometry(shp), add = TRUE, border = 'red')  # Add Murcia region as an overlay

load("BA_modis_200101_202112_10.RData")
BA_within_shp <- BA[lon_indices, lat_indices, ]
BA_MODIS <- mask_domain(BA_within_shp, inout_matrix) 
image.plot(lon_within_shp, lat_within_shp, apply(log(BA_MODIS+1),c(1,2),sd,na.rm=T), main = "In/Out Mask", xlab = "Longitude", ylab = "Latitude")
plot(st_geometry(shp), add = TRUE, border = 'red')  # Add Murcia region as an overlay

fname <- file.path('BA_modis_nat_200101_202212_10.nc')
obs.nc <- nc_open(fname)
BA <- ncvar_get(obs.nc, "burntAreaAll")
# Original dimensions of BA
original_dim <- dim(BA)
# Create an array with dimensions (lon, lat+1, time) and fill with NA
BA_extended <- array(NA, dim = c(original_dim[1], original_dim[2] + 1, original_dim[3]))
# Copy the original BA values into the new array
BA_extended[, 1:original_dim[2], ] <- BA
BA_within_shp=BA_extended
image.plot(lon_within_shp,lat_within_shp,apply(log10(BA_within_shp+1),c(1,2),mean,na.rm=TRUE))
plot(wrld_simpl, add = TRUE)
BA_MODIS_nat <- mask_domain(BA_within_shp, inout_matrix) 
image.plot(lon_within_shp, lat_within_shp, apply(log(BA_MODIS_nat+1),c(1,2),mean,na.rm=T), main = "In/Out Mask", xlab = "Longitude", ylab = "Latitude")
plot(st_geometry(shp), add = TRUE, border = 'red')  # Add Murcia region as an overlay
dim(BA_MODIS_nat)

fname <- file.path('BA_gfed5_2001-2020_1_Med.nc')
obs.nc <- nc_open(fname)
lon <- obs.nc$dim$lon$vals
lat <- obs.nc$dim$lat$vals
lon_within_shp <- lon[lon >= lon_min & lon <= lon_max]
lat_within_shp <- lat[lat >= lat_min & lat <= lat_max]
lon_indices <- which(lon >= lon_min & lon <= lon_max)
lat_indices <- which(lat >= lat_min & lat <= lat_max)
total <- ncvar_get(obs.nc, "Total")
crop <- ncvar_get(obs.nc, "Crop")
urban <- ncvar_get(obs.nc, "Norm")[,,14,]
BA <- total - crop - urban
BA_within_shp <- BA[lon_indices, lat_indices, ]
BA_GFED5_nat <- mask_domain(BA_within_shp, inout_matrix) 
image.plot(lon_within_shp, lat_within_shp, apply(log(BA_GFED5_nat+1),c(1,2),mean,na.rm=T), main = "In/Out Mask", xlab = "Longitude", ylab = "Latitude")
plot(st_geometry(shp), add = TRUE, border = 'red')  # Add Murcia region as an overlay


# Set the path where you want to save the files
save_path <- "./"  # Replace with your actual path

lon=lon_within_shp
lat=lat_within_shp
# Save longitude and latitude as one .RData file
save(lon, lat, file = file.path(save_path, "lon_lat.RData"))

BA_MCD64A1=BA_MODIS[,,1:dim(BA_FIRECCI51)[3]]*10000 #ha2m^2
BA_GFED5=BA_GFED5*1000000 #km^2_2_m^2
BA_GFED5_nat=BA_GFED5_nat*1000000 #km^2_2_m^2
BA_MCD64A1_nat=BA_MODIS_nat[,,1:dim(BA_FIRECCI51)[3]]

# Save each BA dataset separately as .RData
save(BA_ONFIRE, file = file.path(save_path, "BA_ONFIRE.RData"))
save(BA_FIRECCI51, file = file.path(save_path, "BA_FIRECCI51.RData"))
save(BA_FIRECCI51_nat, file = file.path(save_path, "BA_FIRECCI51_nat.RData"))
save(BA_MCD64A1, file = file.path(save_path, "BA_MCD64A1.RData"))
save(BA_MCD64A1_nat, file = file.path(save_path, "BA_MCD64A1_nat.RData"))
save(BA_GFED5, file = file.path(save_path, "BA_GFED5.RData"))
save(BA_GFED5_nat, file = file.path(save_path, "BA_GFED5_nat.RData"))



## intersect with NUTS1
# Load the NUTS shapefile
nuts_shp <- st_read("nuts1.shp")  # Adjust path as needed
nuts_shp <- st_transform(nuts_shp, CRS.new)

# Define the function to calculate monthly total BA per region
calculate_monthly_totals <- function(nuts_shp, BA, lon, lat) {
  # Array to store monthly totals per region
  monthly_totals <- matrix(NA, nrow = nrow(nuts_shp), ncol = dim(BA)[3])  # Rows: regions, Columns: months
  
  # Loop over each NUTS region
  for (i in seq_len(nrow(nuts_shp))) {
    region <- nuts_shp[i, ]
    
    # Generate grid points and determine which fall within the region
    points <- expand.grid(lon, lat)
    pts <- st_as_sf(points, coords = c("Var1", "Var2"), crs = CRS.new)
    inout <- st_intersects(pts, region, sparse = FALSE)
    
    # Create a mask matrix for the region
    inout[inout == FALSE] <- NA
    inout_matrix <- matrix(as.numeric(inout), nrow = length(lon), ncol = length(lat))
    
    # Mask the BA data for the current region and sum per month
    BA_masked <- mask_domain(BA, inout_matrix)
    monthly_totals[i, ] <- apply(BA_masked, 3, sum, na.rm = TRUE)
  }
  
  return(monthly_totals)
}

# Set the path where you want to save the results
save_path <- "./"  # Adjust to your desired save directory

# Process each dataset, storing monthly totals per region
# Note: lon and lat must be defined before processing each dataset

# ONFIRE dataset
BA_ONFIRE_monthly_totals <- calculate_monthly_totals(nuts_shp, BA_ONFIRE, lon, lat)
save(BA_ONFIRE_monthly_totals, file = file.path(save_path, "BA_ONFIRE_monthly_totals.RData"))

# FIRECCI51 dataset
BA_FIRECCI51_monthly_totals <- calculate_monthly_totals(nuts_shp, BA_FIRECCI51, lon, lat)
save(BA_FIRECCI51_monthly_totals, file = file.path(save_path, "BA_FIRECCI51_monthly_totals.RData"))

# FIRECCI51 natural dataset
BA_FIRECCI51_nat_monthly_totals <- calculate_monthly_totals(nuts_shp, BA_FIRECCI51_nat, lon, lat)
save(BA_FIRECCI51_nat_monthly_totals, file = file.path(save_path, "BA_FIRECCI51_nat_monthly_totals.RData"))

# MCD64A1 dataset
BA_MCD64A1_monthly_totals <- calculate_monthly_totals(nuts_shp, BA_MCD64A1, lon, lat)
save(BA_MCD64A1_monthly_totals, file = file.path(save_path, "BA_MCD64A1_monthly_totals.RData"))

# MCD64A1 natural dataset
BA_MCD64A1_nat_monthly_totals <- calculate_monthly_totals(nuts_shp, BA_MCD64A1_nat, lon, lat)
save(BA_MCD64A1_nat_monthly_totals, file = file.path(save_path, "BA_MCD64A1_nat_monthly_totals.RData"))

# GFED5 dataset
BA_GFED5_monthly_totals <- calculate_monthly_totals(nuts_shp, BA_GFED5, lon, lat)
save(BA_GFED5_monthly_totals, file = file.path(save_path, "BA_GFED5_monthly_totals.RData"))

# GFED5 natural dataset
BA_GFED5_nat_monthly_totals <- calculate_monthly_totals(nuts_shp, BA_GFED5_nat, lon, lat)
save(BA_GFED5_nat_monthly_totals, file = file.path(save_path, "BA_GFED5_nat_monthly_totals.RData"))


