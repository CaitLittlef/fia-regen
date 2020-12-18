## Predict spatially and overlay every year...
# to get cumulative recruitment after all years have unfolded,
# as if with fire 10 yrs in the past.


## This was largely done in 02b, but just repasting here.
# Temporarily re-set working directory to list all files (May - Sept)
# Load, crop, and mask those rasters.
setwd(paste0(tc.dir,"/def_z/"))
def.tifs = list.files(pattern="*5.9.tif", full.names = TRUE)
def.stack <- stack(def.tifs)
wd <- setwd("C:/Users/clittlef/Google Drive/2RMRS/fia-regen/data") # If working with/within drive
def.stack <- def.stack %>% crop(IntWsts) %>% mask(IntWsts)

# Rename; have run full dates then crop 2 digits off of right
names(def.stack) <- paste0("def", right(c(1981:2017),2))

# Take max from those series of yrs: 

# def0507 <-  overlay(def.stack$def05, def.stack$def06, def.stack$def07,
#                     fun=function(x){ max(x,na.rm=T)})
def0608 <-  overlay(def.stack$def06, def.stack$def07, def.stack$def08,
                    fun=function(x){ max(x,na.rm=T)})
def0709 <-  overlay(def.stack$def07, def.stack$def08, def.stack$def09,
                    fun=function(x){ max(x,na.rm=T)})
def0810 <-  overlay(def.stack$def08, def.stack$def09, def.stack$def10,
                    fun=function(x){ max(x,na.rm=T)})
def1911 <-  overlay(def.stack$def09, def.stack$def10, def.stack$def11,
                    fun=function(x){ max(x,na.rm=T)})
def1012 <-  overlay(def.stack$def10, def.stack$def11, def.stack$def12,
                    fun=function(x){ max(x,na.rm=T)})
def1113 <-  overlay(def.stack$def11, def.stack$def12, def.stack$def13,
                    fun=function(x){ max(x,na.rm=T)})
def1214 <-  overlay(def.stack$def12, def.stack$def13, def.stack$def14,
                    fun=function(x){ max(x,na.rm=T)})
def1315 <-  overlay(def.stack$def13, def.stack$def14, def.stack$def15,
                    fun=function(x){ max(x,na.rm=T)})
def1416 <-  overlay(def.stack$def14, def.stack$def15, def.stack$def16,
                    fun=function(x){ max(x,na.rm=T)})
def1517 <-  overlay(def.stack$def15, def.stack$def16, def.stack$def17,
                    fun=function(x){ max(x,na.rm=T)})



par(mfrow=c(1,1))

# Which species?
sp
if (sp == "pipo") rng.r <- pipo.rng else rng.r <- psme.rng
plot(st_geometry(rng.r))
sp

# which explan vars?
pipo.explan.vars # "YEAR.DIFF"     "BALive_brt_m"  "tmax.tc"       "def59_z_max13" " FIRE.SEV"
psme.explan.vars # "YEAR.DIFF"     "BALive_brt_m"  "tmax.tc"       "DUFF_DEPTH_cm"

## What are vars for which I need rasters? Vary year, let tmax.tc change in space, hold others at mean

# Predict only to range; use a terraclimate raster as template
tmax.tc <- raster(paste0(tc.dir,"tmax.1981.2010.tif"))
tmax.tc <- tmax.tc %>% crop(rng.r) %>% mask(rng.r) # Crop to sp range
plot(tmax.tc)
tmax.tc.data <- gplot_data(tmax.tc)
min(tmax.tc.data$value, na.rm = TRUE)
max(tmax.tc.data$value, na.rm = TRUE)
r <- tmax.tc 
extent(tmax.tc)

YEAR.DIFF <- r
# how many years?
# yr <- 1
yr <- 10
# yr <- 15
YEAR.DIFF[! is.na(YEAR.DIFF)] <- yr
extent(YEAR.DIFF)

BALive_brt_m <- r
BALive_brt_m[! is.na(BALive_brt_m)] <- mean(data.brt$BALive_brt_m)
extent(BALive_brt_m)

DUFF_DEPTH_cm <- r
DUFF_DEPTH_cm[! is.na(DUFF_DEPTH_cm)] <- mean(data.brt$DUFF_DEPTH_cm)
extent(DUFF_DEPTH_cm)

FIRE.SEV <- r
FIRE.SEV[! is.na(FIRE.SEV)] <- Mode(data.brt$FIRE.SEV)
extent(FIRE.SEV)

## For deficit, could use average max
def59_z_max13 <- r
def59_z_max13[! is.na(def59_z_max13)] <- mean(data.brt$def59_z_max13) ; defz <- "avg_max"

####

defz <- "max.0608-1517"
# def59_z_max13 <- def0608 %>%
# def59_z_max13 <- def0709 %>%
# def59_z_max13 <- def0810 %>%
# def59_z_max13 <- def0911 %>%
# def59_z_max13 <- def1012 %>%
# def59_z_max13 <- def1113 %>%
# def59_z_max13 <- def1214 %>%
# def59_z_max13 <- def1315 %>%
# def59_z_max13 <- def1416 %>%
def59_z_max13 <- def1517 %>%
  crop(rng.r) %>%
  mask(rng.r) %>%
  extend(rng.r) 


# plot(def59_z_max15)
# extent(def59_z_max15)
if (sp == "pipo") defz <- defz else defz <- "na"
defz


## Yet the underlying names are still remnants of orig raster file
labels(tmax.tc) # gives "tmax.1981.2010"
names(tmax.tc) <- "tmax.tc"
labels(tmax.tc) # gives "tmax.tc"
names(YEAR.DIFF) <- "YEAR.DIFF"
names(BALive_brt_m) <- "BALive_brt_m"
names(def59_z_max13) <- "def59_z_max13"
names(DUFF_DEPTH_cm) <- "DUFF_DEPTH_cm"
names(FIRE.SEV) <- "FIRE.SEV"

## Create brick for prediction (ok to have all b/c each sp will only pull what it needs)
brick <- brick(YEAR.DIFF, BALive_brt_m, tmax.tc, def59_z_max13, DUFF_DEPTH_cm, FIRE.SEV)
names(brick)

## Predict! Below JUST uses first model; instead, run for all 100 and take avg.
# Loop thru to fill list with predictions for each pixel from ALL models
start <- Sys.time() 

preds <- list()
# for (i in 1:5) {
for (i in 1:10) {  
# for (i in 1:100) {
  # preds[[i]] <- stack %>% 
  preds[[i]] <- brick %>%
  raster::predict(models[[i]],
                  n.trees = models[[i]]$n.trees,
                  type = "response") %>%
  gplot_data()
} ; print(Sys.time() - start)


## Combine all predictors into table. *** REFERENCING i in pred loop ABOVE!! ***
# dplyr's bind_rows() avoids duped names unlike do.call(rbind...)
preds.df <- bind_rows(preds) %>% dplyr::select(-variable) # 3 vars
# preds.raw.df <- bind_cols(preds) # 400 vars
nrow(preds.df)/i # 49815 for pipo, 49920 for psme
x <- preds.df$x[1:49815] # for pipo
y <- preds.df$y[1:49815] # for pipo
values.df <- preds.df %>% dplyr::select(-x, -y) 
i
values.df <- as.data.frame(matrix(values.df$value, ncol = i, byrow = FALSE))

# Average all values together
values.df <- values.df %>%
  mutate(value = rowMeans(.)) %>%
  dplyr::select(value)



#### PICK CORRECT ONE BELOW!!
## Append into dataframe that's unique to that year span, here, starting with '06

# values.xy <- as.data.frame(cbind(x, y, values.df)) # first one
values.xy <- cbind(values.xy, values.df$value) # all subsequent 3-year combos

####


## When done running through each of the 3-yr spans

colnames(values.xy) <- c("x", "y", paste0("val", right(c(2006:2015),2), right(c(2008:2017),2)))

values.xy <- values.xy %>%
  mutate(value = rowSums(.[3:12])) %>%
  dplyr::select(x, y, value)

min(values.xy$value, na.rm = T) #  1.691868
max(values.xy$value, na.rm = T) # 2.284129


# Set gradient limits based on sp
sp
if (sp == "pipo") limits <- c(1.6, 2.3) else limits <- c(0.23,0.47)

# display.brewer.pal(8, "Dark2") ; pal.d2
# display.brewer.pal(8, "RdYlBu") ; pal.ryb
# display.brewer.pal(8, "PiYG") ; pal.pg
# display.brewer.pal(8, "PRGn") ; pal.prgn



## Goal: project maps. Folks recommend ggplot-similar tmap.
# refs:
# https://geocompr.robinlovelace.net/adv-map.html
# see colors with this: 
# tmaptools::palette_explorer()


# install.packages("tmap")
# install.packages("shinyjs")
library(tmap)
library(shinyjs)

## Convert 100-run averages into raster for tmap

ready <- rasterFromXYZ(values.xy, crs = crs(rng.r))

plot(ready)

## Pick Albers equal area projection & transform all data.
aea.proj <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-110 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83  +units=m"

IntWsts.aea <- st_transform(IntWsts, crs = aea.proj)
nonIntWest.aea <- st_transform(nonIntWest, crs = aea.proj)
ready.aea <- projectRaster(ready, crs = aea.proj)
# hill.aea <- projectRaster(hill, crs = aea.proj)

# plot(st_geometry(IntWsts))
# plot(st_geometry(IntWsts.aea))
# plot(st_geometry(nonIntWest))
# plot(st_geometry(nonIntWest.aea))
# plot(def.stack$def15)
# plot(def15.aea)
# plot(hill.aea)



## I'll want to control bounding box of map.
# Use IntW, expanded slightly.
# https://www.jla-data.net/eng/adjusting-bounding-box-of-a-tmap-map/
(bbox <- st_bbox(IntWsts.aea))
bbox_new <- bbox
bbox_new[1] <- (bbox[1] - 20000) #xmin 
bbox_new[3] <- (bbox[3] + 20000) #xmas
bbox_new[2] <- (bbox[2] - 20000) #ymin
bbox_new[4] <- (bbox[4] + 20000) #ymax

bbox_new <- bbox_new %>%  # take the bounding box ...
  st_as_sfc() # ... and make it a sf polygon

# display.brewer.all(colorblindFriendly = TRUE)
# display.brewer.pal(8, "Dark2") ; pal.d2
# display.brewer.pal(8, "RdYlBu") ; pal.ryb
# display.brewer.pal(8, "PiYG") ; pal.pg
# display.brewer.pal(8, "PRGn") ; pal.prgn



## Create map. Cannot figure out how to get negative values on bottom in legend. Fix in pwrpt
# Or not show hill legend -- so no hill.
map <- # by default, set master to establish bbox, projection, etc (default = 1st raster)
  tm_shape(IntWsts.aea, is.master = TRUE, bbox = bbox_new) + # master to rule bbox, proj
  tm_fill("gray98") + # for holes in raster
  # add in deficit values with reverse palette; may make transparent with alpha
  tm_shape(ready.aea) + tm_raster(palette = "YlGn", #"PRGn",
                                  # tm_shape(def16.aea) + tm_raster(palette = "-RdYlBu",
                                  # tm_shape(def17.aea) + tm_raster(palette = "-RdYlBu",
                                  style = "cont",
                                  title = "Prob. juv.\npresence") +#, alpha = 0.85) +
  # add in non-Interior West states with light grey fill
  tm_shape(nonIntWest.aea) + tm_borders(lwd=1.5) + tm_fill("gray90") +
  # add in Interior West states with no fill
  tm_shape(IntWsts.aea) + tm_borders(lwd=1.5) + 
  tm_layout(legend.bg.color = "white",
            legend.frame = TRUE,
            legend.title.size = 0.8,
            legend.text.size = 0.6,
            legend.show = TRUE,
            # legend.only = TRUE,
            # legend.outside = TRUE,
            legend.position = c(0.01, 0.01)) ; map


## Save as pdf by version
# v <- 1
pdf(paste0(out.dir, "pred_map_multiyr_v",v, "_", currentDate,".pdf"),
    width = 3, height = 4) ; v <- v+1
map
dev.off()


defz


data.psme %>%
  group_by(STATECD.x) %>%
  summarise(meanBA = mean(BALive_psme_m)) %>%
  dplyr::select(STATECD.x, meanBA)

data.pipo %>%
  group_by(STATECD.x) %>%
  summarise(meanBA = mean(BALive_pipo_m)) %>%
  dplyr::select(STATECD.x, meanBA)

data.pipo %>%
  group_by(STATECD.x) %>%
  summarise(meanZ = mean(def.2017.5.9_z)) %>%
  dplyr::select(STATECD.x, meanZ)


# For smoothing gradient, see discussion here:
# https://stackoverflow.com/questions/28059182/smoothing-out-ggplot2-map
# And tools here: 
# https://cran.r-project.org/web/views/Spatial.html

