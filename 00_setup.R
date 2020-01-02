###########
## SETUP ##
###########

#####################################
## Set working directory to input data
# wd <- setwd("//goshawk.sefs.uw.edu/Space_Lawler/Shared/BackedUp/Caitlin/RMRS")
# wd <- setwd("D:/Shared/BackedUp/Caitlin/RMRS") # If working from within goshawk
wd <- setwd("C:/Users/clittlef/Google Drive/2RMRS/fia-regen/data") # If working with/within drive
out.dir <- "C:/Users/clittlef/Google Drive/2RMRS/fia-regen/output/"
code.dir <- "C:/Users/clittlef/Google Drive/2RMRS/fia-regen/fia-regen/"
## set directory for terraclimate
tc.dir <- "C:/Users/clittlef/Google Drive/2RMRS/fia-regen/data/terraclimate/"

cait <- "//goshawk.sefs.uw.edu/Space_Lawler/Shared/BackedUp/Caitlin"



#####################################
# Install packages if not already installed
required.packages <- c("plyr", "ggplot2", "gridExtra", "raster", "sf", "rgdal", "dplyr",
                       "tidyverse", "maptools", "rgeos", 
                       "partykit", "vcd", "maps", "mgcv", "tmap",
                       "MASS", "pROC", "ResourceSelection", "caret", "broom", "boot",
                       "dismo", "gbm", "usdm", "pscl", "randomForest", "pdp", "classInt", "plotmo",
                       "ggspatial", "lmtest",  "dynatopmodel", "spatialEco")
new.packages <- required.packages[!(required.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)>0) install.packages(new.packages)
rm(required.packages, new.packages)

# Libraries
# library(plyr)
library(ggplot2)
library(gridExtra)
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
library(boot)
library(dismo)
library(gbm)
library(usdm)
library(pscl)
library(randomForest)
library(pdp)
library(classInt)
library(plotmo)
library(ggspatial)
library(lmtest)
library(dynatopmodel)
library(spatialEco)

# rm(GCtorture)

#####################################
# Turn off scientific notation
options(scipen=999) 


#####################################
# Grab date for saving files
currentDate <- Sys.Date()


####################################
## Get background of W states
NAmer <- st_read(dsn = "NorthAmer_StatesProvinces.shp") %>%
  st_buffer(dist = 0) # fix invalid geometries (warning re: lat/long vs. dd)
NAmer <- NAmer[!NAmer$NAME == "Guam",]
NAmer.outline <- st_union(NAmer)
# plot(NAmer.outline)
Wsts.names = c('California', 'Oregon', 'Washington','Idaho', 'Nevada',
        'Montana','Wyoming','Utah','Arizona','New Mexico','Colorado')
IntWsts.names = c('Idaho', 'Nevada','Montana','Wyoming','Utah','Arizona','New Mexico','Colorado')
Wsts <- NAmer[NAmer$NAME %in% Wsts.names, ] # not NAmer@data$NAME
IntWsts <- NAmer[NAmer$NAME %in% IntWsts.names, ]
nonIntWest  <- NAmer[! NAmer$NAME %in% IntWsts.names, ]
crs <- crs(Wsts)
bbox <- st_as_sfc(st_bbox(nonIntWest))

## Load hillshade (already created on goshawk in 'topo - random' folder)
hill <-raster("hill.IntWst.tif")
plot(hill)

## Load pipo & psme range (I think from DAtabasin -- confirm source!!)
pipo.rng <- st_read(dsn = "pinupond.shp")
psme.rng <- st_read(dsn = "pseumenz.shp")
plot(st_geometry(psme.rng))
crs(pipo.rng)

# Set crs to same as 
pipo.rng <- st_transform(x = pipo.rng, crs = crs)
crs(pipo.rng)

# Set crs to same as 
psme.rng <- st_transform(x = psme.rng, crs = crs)
crs(pipo.rng)


## Load TerraClime datasets
# aet <- raster(paste0(tc.dir,"aet.1981.2010.tif"))
# def <- raster(paste0(tc.dir,"def.1981.2010.tif"))
# ppt <- raster(paste0(tc.dir,"ppt.1981.2010.tif"))
# tmax <- raster(paste0(tc.dir,"tmax.1981.2010.tif"))




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

# SE
stdErr <- function(x) {sd(x)/ sqrt(length(x))}


############################################################################################
# Wes palette
# install.packages("wesanderson")
# library(wesanderson)
# pal.d1 <- wes_palette("Darjeeling1")
# pal.d2 <- wes_palette("Darjeeling2")
# pal <- c("#000000", "#F98400",  "#046C9A", "#FF0000", "#00A08A", "#00A08A", "#F98400", "#FF0000", "#00A08A", "#5BBCD6","#F2AD00", "#F98400")
# pal colors are X, X, blue, X, teal, red, X, light blue, yellow, orange

#046C9A # blue
#F98400 # orange



# Color blind palette
# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


# grey, orange, light blue, pine green, yellow, dark blue, red, pink

library(RColorBrewer)
display.brewer.all(7)
display.brewer.pal(7, "Set1")
palette <- brewer.pal(7, "Set1")

display.brewer.all(colorblindFriendly = TRUE)
display.brewer.pal(8, "Dark2")
palette <- brewer.pal(8, "Dark2")


###########################################
theme_caitlin <- function(base_size=12, base_family="sans") {
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size, base_family=base_family)
    + theme(text = element_text(size=12),
            axis.text.x = element_text(color="black", size=10),
            axis.text.y = element_text(color="black", size=10),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.background = element_blank())
  )
}




#######################################################################
## To plot raster in ggplot, extract values into tibble
# ref: https://stackoverflow.com/questions/47116217/overlay-raster-layer-on-map-in-ggplot2-in-r
# Define function to extract raster values into a tibble
gplot_data <- function(x, maxpixels = 50000)  {
  x <- raster::sampleRegular(x, maxpixels, asRaster = TRUE)
  coords <- raster::xyFromCell(x, seq_len(raster::ncell(x)))
  ## Extract values
  dat <- utils::stack(as.data.frame(raster::getValues(x))) 
  names(dat) <- c('value', 'variable')
  
  dat <- dplyr::as.tbl(data.frame(coords, dat))
  
  if (!is.null(levels(x))) {
    dat <- dplyr::left_join(dat, levels(x)[[1]], 
                            by = c("value" = "ID"))
  }
  dat
}





#######################################################################
## From ggExtra -- to specify size of plots (adapted align.plots)

align.plots2 <- function (..., vertical = TRUE, pos = NULL) 
{
  dots <- list(...)
  if (is.null(pos)) pos <- lapply(seq(dots), I)
  dots <- lapply(dots, ggplotGrob)
  ytitles <- lapply(dots, function(.g) editGrob(getGrob(.g, 
                                                        "axis.title.y.text", grep = TRUE), vp = NULL))
  ylabels <- lapply(dots, function(.g) editGrob(getGrob(.g, 
                                                        "axis.text.y.text", grep = TRUE), vp = NULL))
  legends <- lapply(dots, function(.g) if (!is.null(.g$children$legends)) 
    editGrob(.g$children$legends, vp = NULL)
    else ggplot2:::.zeroGrob)
  gl <- grid.layout(nrow = do.call(max,pos))
  vp <- viewport(layout = gl)
  pushViewport(vp)
  widths.left <- mapply(`+`, e1 = lapply(ytitles, grobWidth), 
                        e2 = lapply(ylabels, grobWidth), SIMPLIFY = F)
  widths.right <- lapply(legends, function(g) grobWidth(g) + 
                           if (is.zero(g)) 
                             unit(0, "lines")
                         else unit(0.5, "lines"))
  widths.left.max <- max(do.call(unit.c, widths.left))
  widths.right.max <- max(do.call(unit.c, widths.right))
  for (ii in seq_along(dots)) {
    pushViewport(viewport(layout.pos.row = pos[[ii]]))
    pushViewport(viewport(x = unit(0, "npc") + widths.left.max - 
                            widths.left[[ii]], width = unit(1, "npc") - widths.left.max + 
                            widths.left[[ii]] - widths.right.max + widths.right[[ii]], 
                          just = "left"))
    grid.draw(dots[[ii]])
    upViewport(2)
  }
}

# 
