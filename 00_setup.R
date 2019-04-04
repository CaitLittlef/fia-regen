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
                       "MASS", "pROC", "ResourceSelection", "caret", "broom",
                       "dismo", "pscl", "randomForest", "pdp", "classInt", "plotmo",
                       "ggspatial")
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
library(broom)
library(dismo)
library(pscl)
library(randomForest)
library(pdp)
library(classInt)
library(plotmo)
library(ggspatial)

# rm(GCtorture)

#####################################
# Turn off scientific notation
options(scipen=999) 


#####################################
# Grab date for saving files
currentDate <- Sys.Date()


####################################
## Get background of W states
# NAmer <- st_read(dsn = "//goshawk.sefs.uw.edu/Space_Lawler/Shared/BackedUp/Caitlin/boundaries/NorthAmer_StatesProvinces.shp") %>% 
#   st_buffer(dist = 0) # fix invalid geometries (warning re: lat/long vs. dd)
NAmer <- st_read(dsn = "NorthAmer_StatesProvinces.shp") %>% 
  st_buffer(dist = 0) # fix invalid geometries (warning re: lat/long vs. dd)
NAmer <- NAmer[!NAmer$NAME == "Guam",]
NAmer.outline <- st_union(NAmer)
plot(NAmer.outline)
Wsts.names = c('California', 'Oregon', 'Washington','Idaho', 'Nevada',
        'Montana','Wyoming','Utah','Arizona','New Mexico','Colorado')
IntWsts.names = c('Idaho', 'Nevada','Montana','Wyoming','Utah','Arizona','New Mexico','Colorado')
Wsts <- NAmer[NAmer$NAME %in% Wsts.names, ] # not NAmer@data$NAME
IntWsts <- NAmer[NAmer$NAME %in% IntWsts.names, ]
nonIntWest  <- NAmer[! NAmer$NAME %in% IntWsts.names, ]
# plot(Wsts) # Get multiple b/c multiple attributes
# plot(st_geometry(Wsts)) # Just outline
# crs(Wsts) # "+proj=longlat +datum=NAD83 +no_defs"



#####################################
# Functions

### COEFFICIENT OF VARIATION
CV <- function(x) {100*sd(x) / mean(x)}



############################################################################################
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



############################################################################################
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


############################################################################################
# Text extraction
left = function(text, num_char) {
  substr(text, 1, num_char)
}

mid = function(text, start_num, num_char) {
  substr(text, start_num, start_num + num_char - 1)
}

right = function(text, num_char) {
  substr(text, nchar(text) - (num_char-1), nchar(text))
}


############################################################################################
# Mode
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}




############################################################################################
# Wes palette
install.packages("wesanderson")
library(wesanderson)
pal.d1 <- wes_palette("Darjeeling1")
pal.d2 <- wes_palette("Darjeeling2")
pal <- c("#000000", "#000000", "#046C9A", "#FF0000", "#00A08A", "#00A08A", "#F98400", "#F2AD00", "#D55E00", "#5BBCD6", "#F98400")

#046C9A # blue
#F98400 # orange