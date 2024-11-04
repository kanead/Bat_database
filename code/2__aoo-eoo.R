# Function to calculate EOO (extent of occurrence) and AOO (area of occupancy) for African bats
# Code was written by Abby Rutrough, with some input from Ara, at the African bat red listing workshop in Namibia (Sept 2023)
# The SDMs that serve as the input to this function are Maxent models of African bats prepared by Ara Monadjem
# The database on which the Maxent models were run, was created, compiled and curated by Ara Monadjem

#-------------------------------------------------------------------------------------------------------------------------------------
rm(list = ls()) # removes everything currently held in the R memory
graphics.off() # closes all open graphics windows
# to clear the screen click "control L"
# set.seed(123)
#-------------------------------------------------------------------------------------------------------------------------------------
# libraries
library(raster)
#library(terra)
library(sf)
library(elevatr)
library(tidyverse)
library(changeRangeR)


#-------------------------------------------------------------------------------------------------------------------------------------
# Make empty list
results <- list()

dir <- c("")
sdm.list <- list.files(path=dir, 
                        pattern =".tif", full.names=TRUE)  # pattern = ".asc" for asci files

# Read in the bat location data
setwd("")

bats1 <- read.csv("African Bat Database_Sci Data_Final version 16 Oct 2024.csv") %>% 
  filter(Reject != "Yes") %>% 
  #filter(Island != "Yes") %>% # no need to filter for island because they have been rejected already
  #dplyr::select(-Island) %>% 
  dplyr::select(Order, Genus, Species, Longitude, Latitude) 

bats_new <- bats1 %>% 
   mutate(Species1 = gsub("cf. ","cf..", bats1$Species)) %>%
   unite(Species, Genus, Species1, sep = ".") %>% 
   drop_na(Latitude) %>% group_by(Species) %>%
   filter(n() > 7) %>%
   ungroup()

# compare these species with that used for SDM (Maxent) model 
names <- data.frame(unique(bats_new$Species))
arrange(names, unique.bats_new.Species.)

# Set where to save the rasters and vectors of AOO and EOO
path.AOO.rast <- "/AOO_rasters/"
path.EOO.shp <- "/EOO_shapefiles/"

  
setwd("/Elevation_raster")
elev1 <- raster("elevation_Africa.asc")

AOO_EOO <- function (x) {    
  x.raster <- raster(x)
  x.pts <- bats_new %>% filter(Species == x.raster@data@names)
  # Calculate EOO
  x.EOO <- mcp(x.pts[,3:4])  # unitless measure
  # set the CRS 
  crs(x.EOO) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  area <- area(x.EOO)/1000000
  paste0(area, " km^2") 
  x.raster[x.raster == 0] <- NA
  crs(x.raster) <- "+proj=longlat +datum=WGS84 +no_defs"
  x.crop <- crop(x.raster, x.EOO)
  x.mask <- mask(x.crop, x.EOO)
  x.AOO <- AOOarea(r = x.mask)
  x.elev <- raster::extract(elev1, st_as_sf(x.pts, coords =  c("Longitude", "Latitude"), crs = crs(elev1)), method = "simple")
  x.elev.min <- min(unique(x.elev))
  x.elev.max <- max(unique(x.elev))
  x.pts$PA <- 1
  x.pts.sf <- st_as_sf(x.pts, coords = c("Longitude", "Latitude"), crs = crs("+proj=longlat +datum=WGS84 +no_defs"))
  # create raster
  template <- raster(ext = extend(extent(x.pts.sf), 1), crs = crs("+proj=longlat +datum=WGS84 +no_defs"), resolution = 0.02, vals = 0)
  x.pts.AOO.rast <- raster::rasterize(x = x.pts.sf, y = template, background = NA, field = "PA")
  # calculate aooArea
  x.pts.AOO <- AOOarea(r = x.pts.AOO.rast)
  x.list <- list(x.raster@data@names, x.AOO$area$km2, x.pts.AOO$area$km2, area(x.EOO)/1000000, x.elev.min, x.elev.max)
  names(x.list) <- c("Species", "AOO Maxent", "AOO Known Locations", "EOO", "Minimum Elevation", "Maximum Elevation")
  results[[length(results) + 1]] <- x.list
  writeRaster(x.mask, paste(path.AOO.rast,x.raster@data@names,"AOO", "MaxEnt", sep = ""), format = "GTiff", overwrite=TRUE)
  #write_sf(st_as_sf(x.EOO), driver = "ESRI Shapefile", path.EOO.shp, paste(x.raster@data@names,"_","EOO", sep = "")) this step doesn't run probably because shapefile name has a limit of 10 characters
  return(results)
}

#-------------------------------------------------------------------------------------------------------------------------------------
# run the analysis using lapply

results.table <- lapply(sdm.list, AOO_EOO)
results.df <- data.frame(do.call(rbind, lapply(results.table, data.frame)))
results.df <- apply(results.df, 2, as.character)

#-------------------------------------------------------------------------------------------------------------------------------------
# save the output as a csv file
setwd("/A00_E00 output/")
write.csv(results.df, "AOO_EOO values_6 Aug 2024.csv")




#-------------------------------------------------------------------------------------------------------------------------------------
