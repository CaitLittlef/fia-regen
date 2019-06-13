## Predict spatially

# What are vars for which I need rasters? Vary year, let tmax.tc change in space, hold others at mean
models[[1]]$var.names
# [1] "YEAR.DIFF"     "BALive_brt_m"  "tmax.tc"       "def59_z_max15" "DUFF_DEPTH_cm"

# Predict only to pipo range; use a terraclimate raster as template
tmax.tc <- raster(paste0(tc.dir,"tmax.1981.2010.tif"))
tmax.tc <- tmax.tc %>% crop(pipo.rng) %>% mask(pipo.rng) # Crop to pipo range
r <- tmax.tc 
YEAR.DIFF <- r
YEAR.DIFF[! is.na(YEAR.DIFF)] <- 1 # also do for 10 yrs since fire
BALive_brt_m <- r
BALive_brt_m[! is.na(BALive_brt_m)] <- mean(data.brt$BALive_brt_m)
def59_z_max15 <- r
def59_z_max15[! is.na(def59_z_max15)] <- mean(data.brt$def59_z_max15)
DUFF_DEPTH_cm <- r
DUFF_DEPTH_cm[! is.na(DUFF_DEPTH_cm)] <- mean(data.brt$DUFF_DEPTH_cm)

brick <- brick(YEAR.DIFF, BALive_brt_m, tmax.tc, def59_z_max15, DUFF_DEPTH_cm)

space <- raster::predict(brick, models[[1]], n.trees=models[[1]]$n.trees, type = "response")

> p <- predict(Anguilla_grids, angaus.tc5.lr005, const=add,
               + n.trees=angaus.tc5.lr005$gbm.call$best.trees, type="response")
brick[[1]]



p <- ggplot() +
  # state outlines
  geom_sf(data = NAmer, color = "#808B96", fill = "white") +
  geom_sf(data = IntWsts, color = "#808B96", fill = "#EAECEE") +
  coord_sf(xlim = c(-121, -100), ylim = c(30, 50), expand = FALSE) +
  geom_point(data = data.psme, aes(x = LON_FS, y = LAT_FS, col = node),
             size = 5, alpha = 0.7) +
  scale_color_manual(values = pal[loop.ready],
                     labels = labels) + 
  theme_bw(base_size = 18) +
  theme(panel.grid.major = element_line(color = "#808B96"), # blend lat/long into background
        panel.background = element_rect(fill = "#808B96"),
        axis.title = element_blank(),
        legend.title = element_blank(),
        legend.background = element_rect(color = "#808B96"),
        legend.justification=c(0,0), # defines which side of legend .position coords refer to
        legend.position=c(0,0),
        legend.text=element_text(size=12))
p
# tiff(paste0(out.dir,"psme_tree_map_",currentDate,".tiff"),
#    width = 450, height = 600, units = "px")
# p
# dev.off()
