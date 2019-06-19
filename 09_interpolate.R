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

