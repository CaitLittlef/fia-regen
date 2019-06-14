## Predict spatially

# What are vars for which I need rasters? Vary year, let tmax.tc change in space, hold others at mean
models[[1]]$var.names
# [1] "YEAR.DIFF"     "BALive_brt_m"  "tmax.tc"       "def59_z_max15" "DUFF_DEPTH_cm"

# Predict only to pipo range; use a terraclimate raster as template
tmax.tc <- raster(paste0(tc.dir,"tmax.1981.2010.tif"))
tmax.tc <- tmax.tc %>% crop(pipo.rng) %>% mask(pipo.rng) # Crop to pipo range
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
brick <- brick(YEAR.DIFF, BALive_brt_m, tmax.tc, def59_z_max15, DUFF_DEPTH_cm)
names(brick)

# Predict! This currently just uses first model; run for all 100 and take avg.
space <- raster::predict(brick, models[[1]], n.trees=models[[1]]$n.trees, type = "response")
# plot(space)

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

# Turn deficit raster into table
space.data <- gplot_data(space)

## Plot into map
p <- ggplot() +
  geom_tile(data = space.data, aes(x = x, y = y, fill = value)) +
  # scale_fill_gradient("Probability of juvenile presence",
  #                     low = palette[6], high = palette[1]) +
  scale_fill_gradient("Probability of juvenile presence",
                      low = palette[6], high = palette[1],
                      limits = c(0.15,0.22)) + # SETS SAME GRADIENT YR 1, YR 10
  geom_sf(data = nonIntWest, color = "#808B96", fill = "white") +
  geom_sf(data = IntWsts, color = "#808B96", fill = NA) +
  coord_sf(xlim = c(-121, -100), ylim = c(30, 50), expand = FALSE) +
  theme_bw(base_size = 18) +
  theme(panel.grid.major = element_line(color = "#808B96"), # blend lat/long into background
        panel.background = element_rect(fill = "#808B96"),
        axis.title = element_blank(),
        legend.title = element_blank(),
        legend.background = element_rect(color = "#808B96"),
        legend.justification=c(0,0), # defines which side oflegend .position coords refer to 
        legend.position=c(0,0),
        legend.text=element_text(size=12))
# p
tiff(paste0(out.dir, sp,"_yr",yr,"_pred_map_",currentDate,".tiff"),
   width = 450, height = 600, units = "px")
p
dev.off()



