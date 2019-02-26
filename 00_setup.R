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
                       "dismo", "pscl")
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
library(pscl)

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


#####################################
# Functions

### COEFFICIENT OF VARIATION
# CV <- function(x) {100*sd(x) / mean(x)}


### MORANS I FUNCTION
Moran_tpha <-function(x)
{cbind(x$long, x$lat) %>%
    dist(.) %>%
    as.matrix(.) %>%
    .^(-1) -> temp
  diag(temp) <- 0
  Moran.I(x$tpha, temp)}


Moran_space <-function(x, y)
{cbind(x$long, x$lat) %>%
    dist(.) %>%
    as.matrix(.) %>%
    .^(-1) -> temp
  diag(temp) <- 0
  Moran.I(y, temp)}


### Multiplot function
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


