## Predict spatially

# Which species?
sp
if (sp == "pipo") rng.r <- pipo.rng else rng.r <- psme.rng
plot(st_geometry(rng.r))

# which explan vars?
pipo.explan.vars # "YEAR.DIFF"     "BALive_brt_m"  "tmax.tc"       "def59_z_max15"
psme.explan.vars # "YEAR.DIFF"     "BALive_brt_m"  "tmax.tc"       "DUFF_DEPTH_cm"

## What are vars for which I need rasters? Vary year, let tmax.tc change in space, hold others at mean

# Predict only to range; use a terraclimate raster as template
tmax.tc <- raster(paste0(tc.dir,"tmax.1981.2010.tif"))
tmax.tc <- tmax.tc %>% crop(rng.r) %>% mask(rng.r) # Crop to sp range

# tmax.tc <- tmax.tc %>% crop(Wsts) %>% mask(Wsts) # Crop to western states (maybe not necessary)
r <- tmax.tc 
YEAR.DIFF <- r
# how many years?
yr <- 1
yr <- 10
YEAR.DIFF[! is.na(YEAR.DIFF)] <- yr
BALive_brt_m <- r
BALive_brt_m[! is.na(BALive_brt_m)] <- mean(data.brt$BALive_brt_m)
def59_z_max15 <- r
def59_z_max15[! is.na(def59_z_max15)] <- mean(data.brt$def59_z_max15)
# Alternative -- using super drought year values (at least in n rockies)
# def59_z_max15 <- r
# def59_z_max15[! is.na(def59_z_max15)] <- max(data.pipo$def.2017.5.9_z)
DUFF_DEPTH_cm <- r
DUFF_DEPTH_cm[! is.na(DUFF_DEPTH_cm)] <- mean(data.brt$DUFF_DEPTH_cm)

# Yet the underlying names are still remnants of orig raster file
labels(tmax.tc) # gives "tmax.1981.2010"
names(tmax.tc) <- "tmax.tc"
labels(tmax.tc) # gives "tmax.tc"
names(YEAR.DIFF) <- "YEAR.DIFF"
names(BALive_brt_m) <- "BALive_brt_m"
names(def59_z_max15) <- "def59_z_max15"
names(DUFF_DEPTH_cm) <- "DUFF_DEPTH_cm"

# Load them all into a brick for predictions
if(sp == "pipo") brick <- brick(YEAR.DIFF, BALive_brt_m, tmax.tc, def59_z_max15) else brick <- brick(YEAR.DIFF, BALive_brt_m, tmax.tc, DUFF_DEPTH_cm)
brick <- brick(YEAR.DIFF, BALive_brt_m, tmax.tc, def59_z_max15, DUFF_DEPTH_cm)
names(brick)

# Predict! This currently just uses first model; run for all 100 and take avg.
space <- raster::predict(brick, models[[1]], n.trees=models[[1]]$n.trees, type = "response")
plot(space)


# Turn deficit raster into table (FUNCTION DEFINED AT SET-UP)
space.data <- gplot_data(space)
## Plot into map
p <- ggplot() +
  geom_tile(data = space.data, aes(x = x, y = y, fill = value)) +
  geom_sf(data = nonIntWest, color = "#808B96", fill = "white") +
  geom_sf(data = IntWsts, color = "#808B96", fill = NA) + # NA else covers tile with color.
  scale_fill_gradient("Probability of juvenile presence",
                      low = palette[6], high = palette[1],
                      na.value = "#EAECEE", # sets background IntW states pale grey
                      limits = c(0.15,0.22)) + # pipo, SETS SAME GRADIENT YR 1, YR 10
                      # limits = c(0.24,0.48)) + # psme, SETS SAME GRADIENT YR 1, YR 10
  coord_sf(xlim = c(-121, -100), ylim = c(30, 50), expand = FALSE) +
  theme_bw(base_size = 18) +
  theme(panel.grid.major = element_line(color = "#808B96"), # blend lat/long into background
        axis.title = element_blank(),
        legend.title = element_blank(),
        legend.background = element_rect(color = "#808B96"),
        legend.justification=c(0,0), # defines which side oflegend .position coords refer to 
        legend.position=c(0,0),
        legend.text=element_text(size=12))
p
tiff(paste0(out.dir, sp,"_yr",yr,"_pred_map_",currentDate,".tiff"),
   width = 450, height = 600, units = "px")
p
dev.off()





######################################################################
###WHAT ABOUT SUPER DROUGHTY CONDITIONS? WHAT'S ASSOCIATED Z-SCORE?###
######################################################################

## V. droughty in northern Rockies in 2017 -- tho not crazy in SW.
hist(data.pipo$def.2017.5.9_z)

p <- ggplot() +
  geom_sf(data = NAmer, color = "#808B96", fill = "white") +
  geom_sf(data = IntWsts, color = "#808B96", fill = "#EAECEE") +
  coord_sf(xlim = c(-121, -100), ylim = c(30, 50), expand = FALSE) +
  geom_point(data = data.pipo, aes(x = LON_FS, y = LAT_FS, col = def.2017.5.9_z),
             size = 5, alpha = 0.7) +
  scale_color_gradient(low = "blue", high = "orange") + 
  theme_bw(base_size = 18) +
  theme(panel.grid.major = element_line(color = "#808B96"), # blend lat/long into background
        panel.background = element_rect(fill = "#808B96"),
        axis.title = element_blank(),
        legend.title = element_blank(),
        legend.background = element_rect(color = "#808B96"),
        legend.justification=c(0,0), # defines which side oflegend .position coords refer to 
        legend.position=c(0,0),
        legend.text=element_text(size=12))
p
dev.off()


## Interpolate these values across pipo range
# ref: https://mgimond.github.io/Spatial/interpolation-in-r.html
library(rgdal)
library(tmap)
library(gstat)

# Create an empty grid where n is the total number of cells. spsample gets pt locations.
grd              <- as.data.frame(spsample(z, "regular", n=50000))
names(grd)       <- c("LON_FS", "LAT_FS")
coordinates(grd) <- c("LON_FS", "LAT_FS")
gridded(grd)     <- TRUE  # Create SpatialPixel object
fullgrid(grd)    <- TRUE  # Create SpatialGrid object
plot(grd)
proj4string(grd) = crs
crs(grd)
class(grd)

grd2 <- rasterToPolygons(r, fun=NULL, n=4, na.rm=TRUE, digits=12, dissolve=FALSE)
class(grd2)
grd3 <- st_make_grid(grd2)
class(grd3)

# Turn z-score data into a shapefile
z <- data.pipo %>%
  dplyr::select(def.2017.5.9_z, LAT_FS, LON_FS)
coordinates(z) = ~LON_FS+LAT_FS
proj4string(z) = crs
crs(z)

# Define formula for z-score values in space
f.1 <- as.formula(def.2017.5.9_z ~ LON_FS + LAT_FS) 

# Compute the sample variogram; note that the f.1 trend model is one of the
# parameters passed to variogram(). This tells the function to create the 
# variogram on the de-trended data.
var.smpl <- variogram(f.1, z, cloud = FALSE, cutoff=1000000, width=89900)

# Compute the variogram model by passing the nugget, sill and range values
# to fit.variogram() via the vgm() function.
dat.fit  <- fit.variogram(var.smpl, fit.ranges = FALSE, fit.sills = FALSE,
                          vgm(psill=14, model="Sph", range=590000, nugget=0))

# The following plot allows us to assess the fit
plot(var.smpl, dat.fit, xlim=c(0,1000))


# Perform the krige interpolation (note the use of the variogram model
# created in the earlier step)
dat.krg <- krige( f.1, z, grd, dat.fit)

# Convert kriged surface to a raster object for clipping
r.z <- raster(dat.krg)
res(r.z)
res(YEAR.DIFF)
extent(r.z)
extent(YEAR.DIFF)
r.z <- r.z %>% crop(rng.r) %>% mask(rng.r)

# Re-create brick with this interpolated z-score data
def59_z_max15 <- r.z
plot(def59_z_max15)
plot(YEAR.DIFF)

brick <- brick(YEAR.DIFF, BALive_brt_m, tmax.tc, def59_z_max15, DUFF_DEPTH_cm)
names(brick)

# Predict! This currently just uses first model; run for all 100 and take avg.
space <- raster::predict(brick, models[[1]], n.trees=models[[1]]$n.trees, type = "response")
plot(space)


