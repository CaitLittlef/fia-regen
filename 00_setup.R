###########
## SETUP ##
###########

#####################################
## Set working directory to input data
# wd <- setwd("//goshawk.sefs.uw.edu/Space_Lawler/Shared/BackedUp/Caitlin/RMRS")
# wd <- setwd("D:/Shared/BackedUp/Caitlin/RMRS") # If working from within goshawk
wd <- setwd("C:/Users/clittlef/Google Drive/2RMRS/fia-regen/data") # If working with/within drive
out.dir <- "C:/Users/clittlef/Google Drive/2RMRS/fia-regen/output/"


#####################################
# Install packages if not already installed
required.packages <- c("ggplot2", "raster", "sf", "rgdal", "dplyr",
                       "tidyverse", "maptools", "rgeos", 
                       "partykit", "vcd", "maps", "mgcv", "tmap",
                       "MASS", "pROC", "ResourceSelection", "caret", "boot",
                       "dismo")
new.packages <- required.packages[!(required.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)>0) install.packages(new.packages)
rm(required.packages, new.packages)

# Libraries
library(ggplot2)
library(raster)
# library(sp)
library(sf)
library(rgdal)
library(dplyr)
library(tidyverse)
library(maptools)
library(rgeos)
library(partykit)
library(vcd)
library(maps)
library(mgcv)
library(tmap)
library(MASS)
library(pROC)
library(ResourceSelection)
library(caret)
library(boot)
library(dismo)

# rm(GCtorture)

#####################################
# Turn off scientific notation
options(scipen=999) 


#####################################
# Grab date for saving files
currentDate <- Sys.Date()


####################################
## Get background of W states
NAmer <- st_read(dsn = "//goshawk.sefs.uw.edu/Space_Lawler/Shared/BackedUp/Caitlin/boundaries/NorthAmer_StatesProvinces.shp") %>% 
  st_buffer(dist = 0) # fix invalid geometries (warning re: lat/long vs. dd)
sts = c('California', 'Oregon', 'Washington','Idaho', 'Nevada',
        'Montana','Wyoming','Utah','Arizona','New Mexico','Colorado')
Wsts <- NAmer[NAmer$NAME %in% sts, ] # not NAmer@data$NAME
plot(Wsts) # Get multiple b/c multiple attributes
plot(st_geometry(Wsts)) # Just outline
crs(Wsts) # "+proj=longlat +datum=NAD83 +no_defs"

