###########
## SETUP ##
###########

#####################################
## Set working directory to input data
# wd <- setwd("//goshawk.sefs.uw.edu/Space_Lawler/Shared/BackedUp/Caitlin/RMRS")
# wd <- setwd("D:/Shared/BackedUp/Caitlin/RMRS") # If working from within goshawk
wd <- setwd("C:/Users/clittlef/Dropbox/RMRS/fia-regen/data") # If working with/within dropbox



#####################################
# Install packages if not already installed
required.packages <- c("ggplot2", "raster", "sp", "rgdal", "dplyr",
                       "tidyverse", "maptools", "rgeos", 
                       "party", "vcd", "maps", "mgcv", "tmap")
new.packages <- required.packages[!(required.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)>0) install.packages(new.packages)
rm(required.packages, new.packages)

# Libraries
library(ggplot2)
library(raster)
library(sp)
library(rgdal)
library(dplyr)
library(tidyverse)
library(maptools)
library(rgeos)
library(party)
library(vcd)
library(maps)
library(mgcv)
library(tmap)



#####################################
# Turn off scientific notation
options(scipen=999) 
