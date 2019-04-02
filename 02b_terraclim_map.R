## Map climatic conditions in study area

## set directory for terraclimate
tc.dir <- "C:/Users/clittlef/Google Drive/2RMRS/fia-regen/data/terraclimate/"
## Load TerraClime datasets
def <- raster(paste0(tc.dir,"def.1981.2010.tif"))

# crop & mask to study area
def.IntWest <- crop(def, IntWsts) # Crops to bounding coordinates
def.IntWest <- mask(def.IntWest, IntWsts) # Sets NA to all outside IntWest 
plot(def.IntWest)

# ref re: plotting rasters in ggplot
# https://stackoverflow.com/questions/47116217/overlay-raster-layer-on-map-in-ggplot2-in-r

# Define functio to extract raster values into a tibble
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
def.data <- gplot_data(def.IntWest)

p <- ggplot() +
  geom_tile(data = def.data, aes(x = x, y = y, fill = value)) +
  scale_fill_gradient("Deficit (mm)",
                      low = '#046C9A', high = '#F98400',
                      na.value = NA) +
  geom_sf(data = nonIntWest, color = "#808B96", fill = "white") +
  geom_sf(data = IntWsts, color = "#808B96", fill = NA) +
  coord_sf(xlim = c(-121, -100), ylim = c(30, 50), expand = FALSE) +
  theme_bw(base_size = 12) +
  theme(panel.grid.major = element_line(color = "#808B96"), # blend lat/long into background
        panel.background = element_rect(fill = "#808B96"),
        axis.title = element_blank(),
        # legend.title = element_blank(),
        legend.background = element_rect(color = "#808B96"),
        legend.justification=c(0,0), # defines which side oflegend .position coords refer to 
        legend.position=c(0,0))
p
# tiff(paste0(out.dir,"def_map_",currentDate,".tiff"),
#    width = 450, height = 600, units = "px")
# p
# dev.off()
