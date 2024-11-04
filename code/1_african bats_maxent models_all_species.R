# Code to run maxent models for each African bat species - in all Africa
# Ara Monadjem, 4 August 2024
# Note that Adam developed the loop for this analysis
# regularization multiplier set = 1 (change accordingly)

#-------------------------------------------------------------------------------------------------------------------------------------
rm(list = ls()) # removes everything currently held in the R memory
graphics.off() # closes all open graphics windows
# to clear the screen click "control L"
# set.seed(123)
#-------------------------------------------------------------------------------------------------------------------------------------
# libraries
options(java.parameters = "-Xmx4g") 
library(usdm)
library(dismo)
# library(rgeos)
library(sf)
library(raster)
library(terra)
library(spatialEco)
library(rJava)
# library(rgdal)
library(tidyverse)
library(conflicted)
conflicts_prefer(dplyr::filter)

#-------------------------------------------------------------------------------------------------------------------------------------
# setting up Java (does not seem to be necesssary anymore?)

# jar = paste(system.file(package="dismo"), "/java/maxent.jar", sep='')
# system.file("java", package="dismo")
# # Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre7')

#-------------------------------------------------------------------------------------------------------------------------------------
# Add 19 Bioclim, elevation, elevation roughness and ecoregions variables 
# VIF analysis (see below, next section) resulted in these: alt, bio2, bio3, bio8, bio9, bio13, bio14, 
# bio15, bio18, bio19, altrough, ecoregions

# currently using 2.5min grids
# see script "African bats_Maxent models_preparing Africa-wide BIOCLIM variables.R"
# setwd("C:/Dropbox/Ara/Publications/Mss - current/Bats - EOO and AOO values for African bats/Bioclim 2021_ver2.1_Africa")

alt = raster("data/wc2.1_30s_elev/wc2.1_30s_elev.tif")
bio2 = raster("data/wc2.1_30s_bio/wc2.1_30s_bio_2.tif") # 2
bio3 = raster("data/wc2.1_30s_bio/wc2.1_30s_bio_3.tif") # 3
bio8 = raster("data/wc2.1_30s_bio/wc2.1_30s_bio_8.tif") # 8
bio9 = raster("data/wc2.1_30s_bio/wc2.1_30s_bio_9.tif") # 9
bio13 = raster("data/wc2.1_30s_bio/wc2.1_30s_bio_13.tif") # 13
bio14 = raster("data/wc2.1_30s_bio/wc2.1_30s_bio_14.tif") # 14
bio15 = raster("data/wc2.1_30s_bio/wc2.1_30s_bio_15.tif") # 15
bio18 = raster("data/wc2.1_30s_bio/wc2.1_30s_bio_18.tif") # 18
bio19 = raster("data/wc2.1_30s_bio/wc2.1_30s_bio_19.tif") # 19

# You can then use the "alt2_5" variable to create the rasterstack (below)


africa1 <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf", continent = 'africa')
africa <- st_as_sf(africa1 %>% filter(!admin %in% c("Comoros", "Madagascar", "Cape Verde")), crs = 4326) %>% # remove islands
  st_crop(c(xmin=-20, xmax= 52, ymin= -36, ymax= 37))

# # need to use resample() to change resolution of bioclim variables


# alt
alt_crop <- terra::crop(alt, africa, mask = TRUE)
alt_mask <- terra::mask(alt_crop, africa)
res(alt_mask)  # check current resolution (= 0.04166667 or 2.5 min); at equator = 4.6km (1 degree = 110.567km)
r2 <- alt_mask
res(r2) <- 0.022610   # (i.e. 2.5km at equator) 2.5/110.567
alt_mask2_5 <- terra::resample(alt_mask, r2)
res(alt_mask2_5) 

# bio2
bio2_crop <- terra::crop(bio2, africa, mask = TRUE)
bio2_mask <- terra::mask(bio2_crop, africa)
res(bio2_mask)  # check current resolution (= 0.04166667 or 2.5 min); at equator = 4.6km (1 degree = 110.567km)
r2 <- bio2_mask
res(r2) <- 0.022610   # (i.e. 2.5km at equator) 2.5/110.567
bio2_mask2_5 <- terra::resample(bio2_mask, r2)
res(bio2_mask2_5) 

# bio3
bio3_crop <- terra::crop(bio3, africa, mask = TRUE)
bio3_mask <- terra::mask(bio3_crop, africa)
res(bio3_mask)  # check current resolution (= 0.04166667 or 2.5 min); at equator = 4.6km (1 degree = 110.567km)
r2 <- bio3_mask
res(r2) <- 0.022610   # (i.e. 2.5km at equator) 2.5/110.567
bio3_mask2_5 <- terra::resample(bio3_mask, r2)
res(bio3_mask2_5) 

# bio8
bio8_crop <- terra::crop(bio8, africa, mask = TRUE)
bio8_mask <- terra::mask(bio8_crop, africa)
res(bio8_mask)  # check current resolution (= 0.04166667 or 2.5 min); at equator = 4.6km (1 degree = 110.567km)
r2 <- bio8_mask
res(r2) <- 0.022610   # (i.e. 2.5km at equator) 2.5/110.567
bio8_mask2_5 <- terra::resample(bio8_mask, r2)
res(bio8_mask2_5) 

# bio9
bio9_crop <- terra::crop(bio9, africa, mask = TRUE)
bio9_mask <- terra::mask(bio9_crop, africa)
res(bio9_mask)  # check current resolution (= 0.04166667 or 2.5 min); at equator = 4.6km (1 degree = 110.567km)
r2 <- bio9_mask
res(r2) <- 0.022610   # (i.e. 2.5km at equator) 2.5/110.567
bio9_mask2_5 <- terra::resample(bio9_mask, r2)
res(bio9_mask2_5) 

# bio13
bio13_crop <- terra::crop(bio13, africa, mask = TRUE)
bio13_mask <- terra::mask(bio13_crop, africa)
res(bio13_mask)  # check current resolution (= 0.04166667 or 2.5 min); at equator = 4.6km (1 degree = 110.567km)
r2 <- bio13_mask
res(r2) <- 0.022610   # (i.e. 2.5km at equator) 2.5/110.567
bio13_mask2_5 <- terra::resample(bio13_mask, r2)
res(bio13_mask2_5) 

# bio14
bio14_crop <- terra::crop(bio14, africa, mask = TRUE)
bio14_mask <- terra::mask(bio14_crop, africa)
res(bio14_mask)  # check current resolution (= 0.04166667 or 2.5 min); at equator = 4.6km (1 degree = 110.567km)
r2 <- bio14_mask
res(r2) <- 0.022610   # (i.e. 2.5km at equator) 2.5/110.567
bio14_mask2_5 <- terra::resample(bio14_mask, r2)
res(bio14_mask2_5) 

# bio15
bio15_crop <- terra::crop(bio15, africa, mask = TRUE)
bio15_mask <- terra::mask(bio15_crop, africa)
res(bio15_mask)  # check current resolution (= 0.04166667 or 2.5 min); at equator = 4.6km (1 degree = 110.567km)
r2 <- bio15_mask
res(r2) <- 0.022610   # (i.e. 2.5km at equator) 2.5/110.567
bio15_mask2_5 <- terra::resample(bio15_mask, r2)
res(bio15_mask2_5) 

# bio18
bio18_crop <- terra::crop(bio18, africa, mask = TRUE)
bio18_mask <- terra::mask(bio18_crop, africa)
res(bio18_mask)  # check current resolution (= 0.04166667 or 2.5 min); at equator = 4.6km (1 degree = 110.567km)
r2 <- bio18_mask
res(r2) <- 0.022610   # (i.e. 2.5km at equator) 2.5/110.567
bio18_mask2_5 <- terra::resample(bio18_mask, r2)
res(bio18_mask2_5) 

# bio19
bio19_crop <- terra::crop(bio19, africa, mask = TRUE)
bio19_mask <- terra::mask(bio19_crop, africa)
res(bio19_mask)  # check current resolution (= 0.04166667 or 2.5 min); at equator = 4.6km (1 degree = 110.567km)
r2 <- bio19_mask
res(r2) <- 0.022610   # (i.e. 2.5km at equator) 2.5/110.567
bio19_mask2_5 <- terra::resample(bio19_mask, r2)
res(bio19_mask2_5) 

# Prepare raster stack
Env_final <- stack(
  alt_mask2_5,
  bio2_mask2_5,
  bio3_mask2_5, 
  bio8_mask2_5, 
  bio9_mask2_5, 
  bio13_mask2_5, 
  bio14_mask2_5,
  bio15_mask2_5, 
  bio18_mask2_5, 
  bio19_mask2_5) 




#-------------------------------------------------------------------------------------------------------------------------------------
# Call species occurrence records

#setwd("C:/Dropbox/Ara/Publications/Mss - current/Bats - EOO and AOO values for African bats/R analyses")

# setwd("C:/Dropbox/Ara/Publications/Mss - current/Bats - EOO and AOO values for African bats/Manuscript/Scientific Data/Final African Bat Database")

bats1 <- read.csv("data/African Bat Database_Sci Data_Final version 16 Oct 2024.csv") %>% 
  unite(GenSp, Genus, Species) %>% 
  dplyr::select(GenSp, Longitude, Latitude) %>% 
  rename(Species = GenSp) %>% # rename GenSp to Species
  drop_na()  # just a few specimens without lat/long (mostly types)

# Check the number of records in database
length(bats1$Longitude) 
SppALL <- unique(bats1$Species)

# remove species with less than 5 records and those from Islands (for modelling reasons)
bats2 <- bats1 %>% 
  group_by(Species) %>%
  filter(n() > 7) %>%
  ungroup()

# compare lengths of database with and without < 7 records and on island (< 300 records)
length(bats2$Longitude) 

bats <- as.data.frame(bats2)
# bats <- na.omit(bats4)  # not necessary since we've already used drop_na()
length(bats$Longitude)

# find out the species number (sequence) in Maxent - 213 species with > 6 records
bat.seq <- bats %>% distinct(Species, .keep_all = TRUE) %>% 
  mutate(Seq = seq(1, length(Longitude))) %>% 
  dplyr::select(Species, Seq)


#-------------------------------------------------------------------------------------------------------------------------------------
# The maxent model (for loop)

# Set output path so that models do not overwrite each other

# Identify the names of the various models ie species of bats
TaxonName <- unique(bats$Species)
# # do this for a subset of the species e.g. Laephotis
# TaxonName1 <- bats %>% filter(grepl("Laephotis", Species, ignore.case = TRUE))  
# TaxonName <- unique(TaxonName1$Species)

# Store results for each species model in a list
Results1 <- list()

# bug causing error with scipen 
# https://stackoverflow.com/questions/76889909/support-with-r-error-code-error-in-if-getoptionscipen-mindigits
options(scipen = 99)

TaxonName[94]

dir <- c("") # avoid uploading to dropbox due to large size of files
# start of loop
for(i in 1:208){#   length(TaxonName)){
#for(i in 204:208) { # use this if you want to pass a selection of species; 
  maxent_out <- paste0(dir, TaxonName[i])  # this will put the maxent output for each bat species in its own folder
  #maxent_out <- dir  # this will put the maxent output for all bats into a single folder - THIS IS PREFERED FOR RECLASSIFICAITON; but doesn't work
  #selTaxon <- which(bats$Species == TaxonName[i])
  selTaxon <- bats[bats$Species == TaxonName[i], ]
  bats_max <- selTaxon[,2:3]

    # remove duplicates within a pixel (cell)
  cells <- cellFromXY(Env_final, bats_max)
  dups <- duplicated(cells)
  bats_occ <- bats_max[!dups, ]   # the ! symbol removes rows with "dups" - defined in previous row
  
  # Create "study area" by using buffer around occurrence points
  bats_max_p <- SpatialPoints(bats_max, proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
  bats_buff <- buffer(bats_max_p,100000)  # adds a buffer (in meters), that will be used to create the "study area" for background points
  bats_buff1 <- crop(bats_buff, africa)  # crop out areas out in the ocean
  bats_buff2 <- rasterize(bats_buff1, Env_final) # convert to raster (for later use to select background points - ie random pseudo-absence points)
  bats_buff_final <- mask(bats_buff2, Env_final) # mask buffer to ensure that it has same extent as enviro layers

  # run Maxent 

  # training and test samples (set seed to keep the same selection)
  set.seed(144)
  selected <- sample(1:nrow(bats_occ), nrow(bats_occ)*0.75)
  bats_train <- bats_occ[selected, ]
  bats_test <- bats_occ[-selected, ]

  # generate background (random) points - 10,000 in this case - and set seed so it is reproducible
  set.seed(144)
  backg1 <- randomPoints(bats_buff_final, 10000, bats_max,tryf = 100)  # NB - changed to 1000 for preliminary run

  # To run maxent model using tabular approach (ie points with associated enviro conditions) - hence need to extract()
  # But need these values to evaluate and calculate thresholds (see below)
  p <- raster::extract(Env_final, bats_train) # extract enviro variables for training data
  p_test <- raster::extract(Env_final, bats_test) # extract enviro variables for test data
  a <- raster::extract(Env_final, backg1) # extract enviro variables for background data
  pa <- c(rep(1, nrow(p)), rep(0, nrow(a))) # assign 1s and 0s for training conditions and background respectively
  pder <- as.data.frame(rbind(p,a)) # this is enviro conditions at all sites (training and background)

  # Alternative is to run Maxent using spatial approach (outputs are identical)
  model <- maxent(
                  x = Env_final, # enviro variables
                  p = bats_max, # presence/absence data (1s and 0s)
                  path = maxent_out,
 # #                 args = prepPara(responsecurves=TRUE,                # this is useful but needs function prepPara
 #                                          userfeatures="LQ",
 #                                          betamultiplier=1))
                  args = c(
                    # logistic output
                    'outputformat=logistic',
                    # 'autofeatures=true'
                    'autofeature=false',
                    # using hinge-only (see Frans et al. 2017)
                    'linear=false',
                    'quadratic=false',
                    'product=false',
                    'threshold=false',
                    'hinge=true',
                    # extract output data 
                    'responsecurves=true',   # consider disabling this line to save time running MAXENT
                    'jackknife=true',        # consider disabling this line to save time running MAXENT
                    'writeplotdata=true',    # consider disabling this line to save time running MAXENT
                    # other settings
                    'askoverwrite=false',
                    'removeduplicates=false',
                    'writeclampgrid=true',
                    'writemess=false',
                    'randomseed=false',
                    # select 20% for testing
                    'randomtestpoints=20',
                    # for model projection
                    'writebackgroundpredictions=false',
                    # set output file type
                    "outputfiletype=asc"
                  ),)   # remember to remove the second bracket if factors = (below) is turned on
                   # list categorical variables
                  #factors=c('ecoregions')) - its important to turn this off when there are no categorical variables

#  model@results # this gives full details of model including thresholds

  # create maps of predicted distribution (maxent model, environmental variables)
#  predict <- predict(model, Env_final) # this works fine, but better use code below for more control over outputs
  predict <- predict(
                    object=model,
                    x=Env_final,
                    filename=paste0(maxent_out, "\\", TaxonName[i], ".asc"),  
                    overwrite=TRUE,
                    )
  
  # # Save map as image - no need for this png file (it might speed up the process?)
  # ttl <- paste0(maxent_out)
  # png(width = 1080, height = 600,
  #     paste0(maxent_out, "/", TaxonName[i], ".png"), res=100)   
  # print(spplot(predict, main=ttl))
  # dev.off()

  # evaluate the model 
  eval_train <- dismo::evaluate(p = p, a = a, model = model)
  eval_test <- dismo::evaluate(p = p_test, a = a, model = model)

  # use max specificity and sensitivity threshold to plot predicted distribution as binary (presence/absence) map
  threshold <- threshold(eval_train, "spec_sens")

 
   
  # Gather results as a list
  List <- list(
    Species = paste(TaxonName[i]),
    model = model,
    map = predict
  )
  #writeRaster(predict, filename=paste0(maxent_out, "/", TaxonName[i], "binary.asc"), format="ascii",overwrite=TRUE)
# I have redone the path for this binary output so all files go to the SAME folder and don't have "binary" in the name
  #maxent_out1 <- ""
  maxent_out1 <- "" #temporarily save onto Downloads folder
  #writeRaster(predict, filename=paste0(maxent_out1,TaxonName[i], ".asc"), format="ascii",overwrite=TRUE)
  writeRaster(predict, filename=paste0(maxent_out1,TaxonName[i], ".tif"), format="GTiff",overwrite=TRUE)
  # Append results to model list
  Results1[[length(Results1)+1]] <- List
  
}


                   ##########################  END  #########################
#-------------------------------------------------------------------------------------------------------------------------------------

