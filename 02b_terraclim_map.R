## Map climatic conditions in study area

## set directory for terraclimate
tc.dir <- "C:/Users/clittlef/Google Drive/2RMRS/fia-regen/data/terraclimate/"
## Load TerraClime datasets
def <- raster(paste0(tc.dir,"def.1981.2010.tif"))

## Alt, look at one yr (e.g., 2017 super droughty in N. Rockies)
def <- raster(paste0(tc.dir,"/def_z/def.2017.5.9.tif"))
# def <- raster(paste0(tc.dir,"/def_z/def.1997.5.9.tif"))
def <- raster(paste0(tc.dir,"/def_z/def.1998.5.9.tif"))
# e.g., 2012 super droughty towards east
def <- raster(paste0(tc.dir,"/def_z/def.2012.5.9.tif"))

# crop & mask to study area
def.IntWest <- crop(def, IntWsts) # Crops to bounding coordinates
def.IntWest <- mask(def.IntWest, IntWsts) # Sets NA to all outside IntWest 
plot(def.IntWest)

# ref re: plotting rasters in ggplot
# https://stackoverflow.com/questions/47116217/overlay-raster-layer-on-map-in-ggplot2-in-r

# Turn deficit raster into table (function defiend in 00_setup)
def.data <- gplot_data(def.IntWest)
min(def.data$value, na.rm =TRUE) # 1997: 02.85; 1998: -3.22; 2012: -2.54; 2017: -3.12
max(def.data$value, na.rm =TRUE) # 1997: 0.92; 1998: 1.75; 2012:  5.41; 2017: 3


# Create hillshade for overlay (not critical... will take a while on this machine)
# elev <- raster(paste0(cait, "/Elevation_GRID/NA_Elevation/data/NA_Elevation/na_elevation"))
# temp <- as(IntWsts, 'Spatial')
# temp <- spTransform(temp, crs(elev))
# elev.IntWest <- mask(elev, temp) %>% trim() # can't crop b/c extends don't overlap
# slp.rad <- terrain(elev.IntWest, opt = "slope", unit = "radians") %>% projectRaster(def)
# asp.rad <- terrain(elev.IntWest, opt = "aspect", unit = "radians") %>% projectRaster(def)
# hill <- hillShade(slp.rad, asp.rad) # requires radians
                                                                                     
# p <- ggplot() +
#   annotate(geom = 'raster', x = hill.data$x, y = hill.data$y,
#            fill = scales::colour_ramp(c("white", "black"))(hill.data$value),
#            interpolate = TRUE)  +
#   geom_tile(data = s.d12.data, aes(x = x, y = y, fill = value), alpha = 0.7) +



p <- ggplot() + # can use geom_tile, but geom_raster works if all cells same size; includes interpolate
  geom_raster(data = def.data, aes(x = x, y = y, fill = value), interpolate = TRUE) +
  scale_fill_gradient("CMD\nanomaly",
                      low = palette[2], high = palette[6],
                      # limits = c(-3.25, 1.75), # 1997
                      limits = c(-3.25,5.45), # 2012 and 2017
                      # limits = c(-2.5,5.45), # 2012
                      # limits = c(-3.2,3), # 2017
                      na.value = NA) +
  geom_sf(data = nonIntWest, color = "#808B96", fill = "white") +
  geom_sf(data = IntWsts, color = "#808B96", fill = NA) +
  coord_sf(xlim = c(-121, -100), ylim = c(30, 50), expand = FALSE) +
  theme_bw(base_size = 18) +
  theme(panel.grid.major = element_line(color = "#808B96"), # blend lat/long into background
        panel.border = element_rect(fill = NA, color = "black", size = 0.5),
        panel.background = element_rect(fill = "#808B96"),
        axis.title = element_blank(),
        # legend.title = element_blank(),
        legend.background = element_rect(color = "#808B96"),
        legend.justification=c(0,0), # defines which side oflegend .position coords refer to 
        legend.position=c(0,0),
        legend.text=element_text(size=10),
        legend.title = element_text(size=12),
        plot.margin=unit(c(0.5,1.5,1.5,1.5),"cm")) + # top, right, bottom, left
  annotate("text", x = -120.5, y = 49.5, label = "a) 2012", hjust = 0)
  # annotate("text", x = -120.5, y = 49.5, label = "c) 2017", hjust = 0)
p

tiff(paste0(out.dir,"def-z_2012_map_",currentDate,".tiff"),
     width = 450, height = 600, units = "px")
tiff(paste0(out.dir,"def-z_2017_map_",currentDate,".tiff"),
   width = 450, height = 600, units = "px")
p
dev.off()


