## Map climatic conditions in study area

## set directory for terraclimate
tc.dir <- "C:/Users/clittlef/Google Drive/2RMRS/fia-regen/data/terraclimate/"
## Load TerraClime datasets
def <- raster(paste0(tc.dir,"def.1981.2010.tif")) %>% crop(IntWsts) %>% mask(IntWsts) %>% plot()
tmax <- raster(paste0(tc.dir, "tmax.1981.2010.tif")) %>% crop(IntWsts) %>% mask(IntWsts) #%>% plot()
precip <- raster(paste0(tc.dir, "ppt.1981.2010.tif")) %>% crop(IntWsts) %>% mask(IntWsts) %>% plot()


## Find years that have strong spatial graident in deficit
# Temporarily re-set working directory to list all files (May - Sept)
# Load, crop, and mask those rasters.
setwd(paste0(tc.dir,"/def_z/"))
def.tifs = list.files(pattern="*5.9.tif", full.names = TRUE)
def.stack <- stack(def.tifs)
wd <- setwd("C:/Users/clittlef/Google Drive/2RMRS/fia-regen/data") # If working with/within drive
def.stack <- def.stack %>% crop(IntWsts) %>% mask(IntWsts)
# Alt: alt, use sequential lapply, tho won't work with mask.
# def.list <- lapply(def.tifs, raster)
# def.list <- lapply(def.list, crop, y = IntWsts) # For lapply, specify 2nd var in fun as y.
# # For whatever reason, cannot apply mask with lapply, so do in loop
# def.list.2 <- list()
# for(i in (1:length(def.list))){
#   def.list.2[[i]] <- def.list[[i]] %>% mask(IntWsts)
# }
# def.list <- def.list.2 ; rm(def.list.2)

# Rename; have run full dates then crop 2 digits off of right
names(def.stack) <- paste0("def", right(c(1981:2017),2))


#############################################WHICH YRS?########################

# ...select 10-12 and 15-17 as big spatial variabiltiy 
plot(def.stack$def15)
plot(def.stack$def99)
# Alt, could unlist and assign names to each separate raster.

# Take max from those series of yrs: 
# def9395 <-  overlay(def.stack$def93, def.stack$def94, def.stack$def95,
#                     fun=function(x){ max(x,na.rm=T)})
# def1012 <-  overlay(def.stack$def10, def.stack$def11, def.stack$def12,
#                     fun=function(x){ max(x,na.rm=T)})
# def1517 <-  overlay(def.stack$def15, def.stack$def16, def.stack$def17,
#                     fun=function(x){ max(x,na.rm=T)})
plot(def9395)
plot(def1012)
plot(def1517)
# zoom(def1517)


#############################################PLOTTING########################
# Turn deficit raster into table (function defiend in 00_setup)
def.data <- gplot_data(def.stack$def15)
def.data <- gplot_data(def.stack$def16)
def.data <- gplot_data(def.stack$def17)
def.data <- gplot_data(def9395)
def.data <- gplot_data(def1012)
def.data <- gplot_data(def1517)

# What should the limits when plotting be?
min(def.data$value[is.finite(def.data$value)], na.rm =TRUE) # 1997: 02.85; 1998: -3.22; 2012: -2.54; 2017: -3.12
max(def.data$value[is.finite(def.data$value)], na.rm =TRUE) # 1997: 0.92; 1998: 1.75; 2012:  5.41; 2017: 3


# Turn hillshade raster into table (function defined in 00_setup)
# hill.data <- gplot_data(hill)
# Then do somethign with this:
# annotate(geom = 'raster', x = hill.data$x, y = hill.data$y,
#          fill = scales::colour_ramp(c("light grey", "dark grey"))(hill.data$value),
#          interpolate = TRUE)  +


## For overlaying 2 rasters, use annotate and geom_raster to control both colors.
# ref re: plotting rasters in ggplot
# https://stackoverflow.com/questions/47116217/overlay-raster-layer-on-map-in-ggplot2-in-r
# Here, can turn on/off hillshade

# For pix min max, load this (from 05_spp_models_brt_pixel_track.R):
pixels <- read.csv("loc.pixels.csv")
rownames(pixels) <- c("pix.min.1012",
                      "pix.max.1012",
                      "pix.min.1517",
                      "pix.max.1517")

display.brewer.pal(8, "Dark2")
dev.off()
par(mfrow=c(1,1))
def.data <- gplot_data(def.stack$def15); yrlabel <- 2015; p15 <- ggplot() +
# def.data <- gplot_data(def.stack$def16); yrlabel <- 2016; p16 <- ggplot() +
# def.data <- gplot_data(def.stack$def17); yrlabel <- 2017; p17 <- ggplot() +
    geom_raster(data = def.data, aes(x = x, y = y, fill = value), interpolate = TRUE) +
    # geom_tile(data = def.data, aes(x = x, y = y, fill = value)) +
    # geom_sf(data = nonIntWest.aea, color = "#808B96", fill = "white") +
    # geom_sf(data = IntWsts.aea, color = "#808B96", fill = NA) +
    geom_sf(data = nonIntWest, color = "#808B96", fill = "white") +
    geom_sf(data = IntWsts, color = "#808B96", fill = NA) +
    # geom_point(data = pixels["pix.min.1012",], aes(x=x, y=y), color = palette[5], size = 5) + 
    # geom_point(data = pixels["pix.max.1012",], aes(x=x, y=y), color = palette[3], size = 5) + 
    # geom_point(data = pixels["pix.min.1517",], aes(x=x, y=y), color = palette[1], size = 5) +
    # geom_point(data = pixels["pix.max.1517",], aes(x=x, y=y), color = palette[4], size = 5) +
    scale_fill_gradient2("CMD\nanomaly",
                        # low = palette[8], mid = "white", high = "#145adb", #high = palette[4],
                        # low = "#145adb", mid = "white", high = palette[2], 
                        low = palette[3], mid = "white", high = palette[2], 
                        midpoint = 0,
                        limits = c(-3.5,3.5), # 2015
                        # limits = c(-1,5.5), # 2016
                        # limits = c(13,19), # 2017
                        na.value = NA) +
                        # na.value = "#EAECEE")+ # sets background IntW states pale grey
    coord_sf(xlim = c(-121, -100), ylim = c(30, 50), expand = FALSE) +
    theme_bw(base_size = 18) +
    # theme(panel.grid.major = element_line(color = "#808B96"), # blend lat/long into background
    theme(panel.grid.major = element_blank(), # blend lat/long into background
          panel.border = element_rect(fill = NA, color = "black", size = 0.5),
          panel.background = element_rect(fill = "#EAECEE"),
          axis.title = element_blank(),
          legend.background = element_rect(fill = "white", color = "black", size = 0,5),
          # legend.title = element_blank(),
          legend.justification=c(0,0), # defines which side oflegend .position coords refer to 
          legend.position=c(0,0),
          legend.text=element_text(size=10),
          legend.title = element_text(size=12),
          # plot.margin=unit(c(0.5,1.5,1.5,1.5),"cm")) + # top, right, bottom, left
          plot.margin=unit(c(0.5,1.25,0.5,0.5),"cm")) + # top, right, bottom, left
    # annotate("text", x = -120.5, y = 49.5, label = "2010-2012", hjust = 0)
  annotate("text", x = -120.5, y = 49.5, label = paste0(yrlabel), hjust = 0) #+
  # coord_equal()
  # coord_map("albers",lat0=39, lat1=45)


dev.off()
p15
p16
p17

temp <- 2015
temp <- 2016
temp <- 2017



# pdf(paste0(out.dir, "def_map_", temp, "_", currentDate,".pdf"),
png(paste0(out.dir, "def_map_", temp, "_", currentDate,".png"),
    # width = 6, height = 8, units = "cm", res = 300)
    width = 475, height = 600, units = "px", pointsize = 12)
    # width = 3, height = 4)
p15; dev.off()
p16; dev.off()
p17; dev.off()




#################################################non-ggplot maps#####################3
## Goal: project maps. Folks recommend ggplot-similar tmap.
# refs:
# https://geocompr.robinlovelace.net/adv-map.html
# see colors with this: 
# tmaptools::palette_explorer()


# install.packages("tmap")
# install.packages("shinyjs")
library(tmap)
library(shinyjs)

# FIXME: cannot add coordaintes. crs still stuck in m and tm_grid shows as such.
# tm_graticules, which should add coords doesn't seem to exist anymore.



## Pick Albers equal area projection & transform all data.
aea.proj <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-110 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83  +units=m"

IntWsts.aea <- st_transform(IntWsts, crs = aea.proj)
nonIntWest.aea <- st_transform(nonIntWest, crs = aea.proj)
def15.aea <- projectRaster(def.stack$def15, crs = aea.proj)
hill.aea <- projectRaster(hill, crs = aea.proj)

plot(st_geometry(IntWsts))
plot(st_geometry(IntWsts.aea))
plot(st_geometry(nonIntWest))
plot(st_geometry(nonIntWest.aea))
plot(def.stack$def15)
plot(def15.aea)
plot(hill.aea)



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


## Create map. Cannot figure out how to get negative values on bottom in legend.
map <- # by default, set master to establish bbox, projection, etc (default = 1st raster)
  tm_shape(IntWsts.aea, is.master = TRUE, bbox = bbox_new) + # master to rule bbox, proj
  tm_fill("grey40") + # for holes in raster
    # # add in hillshade for study area first with continuous grey gradient
    # tm_shape(hill.aea) + tm_raster(palette = "Greys", style = "cont") +
    # add in deficit values with reverse palette; may make transparent with alpha
    tm_shape(def15.aea) + tm_raster(palette = "-RdYlBu",
                                    style = "cont",
                                    title = "CMD\nanomaly") +#, alpha = 0.85) +
    # add in non-Interior West states with light grey fill
    tm_shape(nonIntWest.aea) + tm_borders(lwd=1.5) + tm_fill("gray90") +
    # add in Interior West states with no fill
    tm_shape(IntWsts.aea) + tm_borders(lwd=1.5) + 
    tm_layout(legend.show = TRUE,
              legend.position = c(0.01, 0.01),
              legend.bg.color = "white",
              legend.title.size = 0.8,
              legend.text.size = 0.6,
              legend.frame = TRUE) ; map


## Save as pdf by version
# v <- 1
pdf(paste0(out.dir, "def_map_2015_v",v, "_", currentDate,".pdf"),
    width = 3, height = 4) ; v <- v+1
map
dev.off()


# Coordinates? tm_graticules() no longer seems to exist. Can't figure out lat/long.
# https://geocompr.github.io/post/2019/tmap-grid/




#################################################STUDY SITES#####################3
## map of study sites
temp.pipo <- data.pipo %>% dplyr::select(UNIQUEID, LAT_FS, LON_FS) %>%
  rename(x = LON_FS, y = LAT_FS) %>%
  mutate(pipo = "pipo")
temp.psme <- data.psme %>% dplyr::select(UNIQUEID, LAT_FS, LON_FS) %>%
  rename(x = LON_FS, y = LAT_FS) %>%
  mutate(psme = "psme")
temp <- full_join(temp.pipo, temp.psme, by = c("UNIQUEID", "x", "y")) %>%
  mutate(sp = ifelse(is.na(pipo), "ponderosa", ifelse(is.na(psme), "Douglas-fir", "both species")))
# Order so "both" are plotted on top
temp <- arrange(temp, desc(sp))

# dummy raster to cover up coordinate lines; plot this as single color raster
# dummy <- def.data %>% dplyr::select(x, y, value)
# dummy$value <- ifelse(is.na(dummy$value, 1, NA))
# ^ nevermind. unnecessary if panel.grid.major= element_blank()

display.brewer.pal(8, "Dark2")
dev.off()
par(mfrow=c(1,1))

p <- ggplot() +
  # geom_raster(data = dummy, aes(x = x, y = y, fill = value), interpolate = TRUE) +
  scale_fill_gradient(low = "#EAECEE", high = "#EAECEE", na.value ="#EAECEE", guide = FALSE) +
  geom_sf(data = nonIntWest, color = "#808B96", fill = "white") +
  geom_sf(data = IntWsts, color = "#808B96", fill = NA) +
  # geom_sf(data = IntWsts, color = "#808B96", fill = "#EAECEE", na.value = NA) + 
  geom_point(data = temp, aes(x=x, y=y, color = sp), size = 3, alpha = 0.5) +
  scale_color_manual("FIA plots used", values = c(palette[1], palette[2], palette[3])) + 
  coord_sf(xlim = c(-121, -100), ylim = c(30, 50), expand = FALSE) +
  theme_bw(base_size = 12) +
  theme(panel.grid.major = element_blank(), # blend lat/long into background
        panel.border = element_rect(fill = NA, color = "black", size = 0.5),
        panel.background = element_rect(fill = "#EAECEE"),
        axis.title = element_blank(),
        legend.background = element_rect(fill = "white", color = "black", size = 0,5),
        # legend.title = element_blank(),
        legend.justification=c(0,0), # defines which side oflegend .position coords refer to 
        legend.position=c(0,0),
        legend.text=element_text(size=10),
        legend.title = element_text(size=12),
        plot.margin=unit(c(0.5,1.25,0.5,0.5),"cm"))  # top, right, bottom, left
dev.off()
p


png(paste0(out.dir,"FIA_plots_used_",currentDate,".png"),
     width = 475, height = 600, units = "px")
pdf(paste0(out.dir,"FIA_plots_used_",currentDate,".pdf"),
    width = 3, height = 5)
p # preview it then save as pdf 8x7
dev.off()




########################################ENVI AMPLITUDE#####################
## What's the envi amplidue over which pipo optimum (14-19 degrees C) occurs?

p <- plot_ly(data.pipo, x = ~tmax.tc, y = ~LAT_FS, z = ~ELEV, color = ~tmax.tc) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'TMAX'),
                      yaxis = list(title = 'LAT'),
                      zaxis = list(title = 'ELEV')))
p

temp <- data.pipo %>% filter(tmax.tc >14 & tmax.tc <19)
min(temp$LAT_FS[temp$tmax.tc >18]) # 32.45029
max(temp$LAT_FS[temp$tmax.tc <15]) # 47.70303
min(temp$ELEV[temp$tmax.tc >18]) # 6248
max(temp$ELEV[temp$tmax.tc <15]) # 9143
