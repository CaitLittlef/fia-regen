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

names(def.stack[[1]])

# Create raster stack from this list
test <- stack(def.list)
plot(test[[1]])
r <- stack(L[[1]])
for(i in 2:length(L)) r <- addLayer(r, L[[i]])

plot(def.list.2[[2]])

def.list <- lapply(def.list, mask, y = IntWsts)
plot(def.list[[1]])

need to specify second var in fun as y = 
%>%lapply(crop(IntWsts)) %>% lapply(mask(IntWsts))
def.list[[1]]
def.list <- lapply(lapply(lapply(def.tifs, raster), crop(IntWsts)), mask(IntWsts))
names(def.list[[1]])

## Look at one yr; 2012 super droughty towards east, 2017 super droughty in N. Rockies
# def91 <- raster(paste0(tc.dir,"/def_z/def.1991.5.9.tif")) %>% crop(IntWsts) %>% mask(IntWsts)
# def92 <- raster(paste0(tc.dir,"/def_z/def.1992.5.9.tif")) %>% crop(IntWsts) %>% mask(IntWsts)
# def93 <- raster(paste0(tc.dir,"/def_z/def.1993.5.9.tif")) %>% crop(IntWsts) %>% mask(IntWsts)
# def94 <- raster(paste0(tc.dir,"/def_z/def.1994.5.9.tif")) %>% crop(IntWsts) %>% mask(IntWsts)
# def95 <- raster(paste0(tc.dir,"/def_z/def.1995.5.9.tif")) %>% crop(IntWsts) %>% mask(IntWsts)
# def96 <- raster(paste0(tc.dir,"/def_z/def.1996.5.9.tif")) %>% crop(IntWsts) %>% mask(IntWsts)
# def97 <- raster(paste0(tc.dir,"/def_z/def.1997.5.9.tif")) %>% crop(IntWsts) %>% mask(IntWsts)
# def98 <- raster(paste0(tc.dir,"/def_z/def.1998.5.9.tif")) %>% crop(IntWsts) %>% mask(IntWsts)
# def99 <- raster(paste0(tc.dir,"/def_z/def.1999.5.9.tif")) %>% crop(IntWsts) %>% mask(IntWsts)
# def00 <- raster(paste0(tc.dir,"/def_z/def.2000.5.9.tif")) %>% crop(IntWsts) %>% mask(IntWsts)
# def01 <- raster(paste0(tc.dir,"/def_z/def.2001.5.9.tif")) %>% crop(IntWsts) %>% mask(IntWsts)
# def02 <- raster(paste0(tc.dir,"/def_z/def.2002.5.9.tif")) %>% crop(IntWsts) %>% mask(IntWsts)
# def03 <- raster(paste0(tc.dir,"/def_z/def.2003.5.9.tif")) %>% crop(IntWsts) %>% mask(IntWsts)
# def04 <- raster(paste0(tc.dir,"/def_z/def.2004.5.9.tif")) %>% crop(IntWsts) %>% mask(IntWsts)
# def05 <- raster(paste0(tc.dir,"/def_z/def.2005.5.9.tif")) %>% crop(IntWsts) %>% mask(IntWsts)
# def06 <- raster(paste0(tc.dir,"/def_z/def.2006.5.9.tif")) %>% crop(IntWsts) %>% mask(IntWsts)
# def07 <- raster(paste0(tc.dir,"/def_z/def.2007.5.9.tif")) %>% crop(IntWsts) %>% mask(IntWsts)
# def08 <- raster(paste0(tc.dir,"/def_z/def.2008.5.9.tif")) %>% crop(IntWsts) %>% mask(IntWsts)
# def09 <- raster(paste0(tc.dir,"/def_z/def.2009.5.9.tif")) %>% crop(IntWsts) %>% mask(IntWsts)
# def10 <- raster(paste0(tc.dir,"/def_z/def.2010.5.9.tif")) %>% crop(IntWsts) %>% mask(IntWsts)
# def11 <- raster(paste0(tc.dir,"/def_z/def.2011.5.9.tif")) %>% crop(IntWsts) %>% mask(IntWsts)
# def12 <- raster(paste0(tc.dir,"/def_z/def.2012.5.9.tif")) %>% crop(IntWsts) %>% mask(IntWsts)
# def13 <- raster(paste0(tc.dir,"/def_z/def.2013.5.9.tif")) %>% crop(IntWsts) %>% mask(IntWsts)
# def14 <- raster(paste0(tc.dir,"/def_z/def.2014.5.9.tif")) %>% crop(IntWsts) %>% mask(IntWsts)
# def15 <- raster(paste0(tc.dir,"/def_z/def.2015.5.9.tif")) %>% crop(IntWsts) %>% mask(IntWsts)
# def16 <- raster(paste0(tc.dir,"/def_z/def.2016.5.9.tif")) %>% crop(IntWsts) %>% mask(IntWsts)
# def17 <- raster(paste0(tc.dir,"/def_z/def.2017.5.9.tif")) %>% crop(IntWsts) %>% mask(IntWsts)


par(mfrow=c(1,2))
par(mfrow=c(2,2))
par(mfrow=c(1,3))
# plot(def91)
# plot(def92)
# plot(def93)
# plot(def94)
# plot(def95)
# plot(def96)
# plot(def97)
# plot(def98)
# plot(def99)
# plot(def00)
# plot(def01)
# plot(def02)
# plot(def03)
# plot(def04)
# plot(def05)
# plot(def06)
# plot(def07)
# plot(def08)
# plot(def09)
# plot(def10)
# plot(def11)
# plot(def12)
# plot(def13)
# plot(def14)
# plot(def15)
# plot(def16)
# plot(def17)


# Take max from those series of yrs: 
def9395 <-  overlay(def93, def94, def95, fun=function(x){ max(x,na.rm=T)})
def1012 <-  overlay(def10, def11, def12, fun=function(x){ max(x,na.rm=T)})
def1517 <-  overlay(def15, def16, def17, fun=function(x){ max(x,na.rm=T)})
plot(def1517)
zoom(def1517)



# Turn deficit raster into table (function defiend in 00_setup)
def.data <- gplot_data(def9395)
def.data <- gplot_data(def1012)
def.data <- gplot_data(def15)
def.data <- gplot_data(def1517)

# What should the limits when plotting be?
min(def.data$value[is.finite(def.data$value)], na.rm =TRUE) # 1997: 02.85; 1998: -3.22; 2012: -2.54; 2017: -3.12
max(def.data$value[is.finite(def.data$value)], na.rm =TRUE) # 1997: 0.92; 1998: 1.75; 2012:  5.41; 2017: 3


# Turn hillshade raster into table (function defined in 00_setup)
# hill.data <- gplot_data(hill)


## For overlaying 2 rasters, use annotate and geom_raster to control both colors.
# ref re: plotting rasters in ggplot
# https://stackoverflow.com/questions/47116217/overlay-raster-layer-on-map-in-ggplot2-in-r
# Here, can turn on/off hillshade
par(mfrow=c(1,1))
p <- ggplot() +
  # annotate(geom = 'raster', x = hill.data$x, y = hill.data$y,
  #          fill = scales::colour_ramp(c("light grey", "dark grey"))(hill.data$value),
  #          interpolate = TRUE)  +
    geom_raster(data = def.data, aes(x = x, y = y, fill = value), interpolate = TRUE) +
    geom_sf(data = nonIntWest, color = "#808B96", fill = "white") +
    geom_sf(data = IntWsts, color = "#808B96", fill = NA) +  
    scale_fill_gradient("Max CMD\nanomaly",
                        low = palette[2], high = palette[6],
                        limits = c(-1.5,3.5), # 2015-2017
                        # limits = c(-1,5.5), # 2010-2012
                        na.value = NA) +
                        # na.value = "#EAECEE")+ # sets background IntW states pale grey
    coord_sf(xlim = c(-121, -100), ylim = c(30, 50), expand = FALSE) +
    theme_bw(base_size = 18) +
    theme(panel.grid.major = element_line(color = "#808B96"), # blend lat/long into background
          panel.border = element_rect(fill = NA, color = "black", size = 0.5),
          panel.background = element_rect(fill = "#EAECEE"),
          axis.title = element_blank(),
          legend.background = element_rect(fill = "white", color = "black", size = 0,5),
          # legend.title = element_blank(),
          legend.justification=c(0,0), # defines which side oflegend .position coords refer to 
          legend.position=c(0,0),
          legend.text=element_text(size=10),
          legend.title = element_text(size=12),
          plot.margin=unit(c(0.5,1.5,1.5,1.5),"cm")) + # top, right, bottom, left
    # annotate("text", x = -120.5, y = 49.5, label = "a) 2010-2012", hjust = 0)
  annotate("text", x = -120.5, y = 49.5, label = "c) 2015-2017", hjust = 0)
  p


# tiff(paste0(out.dir,"def-z_2012_map_",currentDate,".tiff"),
#      width = 475, height = 600, units = "px")
# tiff(paste0(out.dir,"def-z_2017_map_",currentDate,".tiff"),
#    width = 475, height = 600, units = "px")
# tiff(paste0(out.dir,"def-z_1012_map_",currentDate,".tiff"),
#      width = 475, height = 600, units = "px")
tiff(paste0(out.dir,"def-z_1517_map_",currentDate,".tiff"),
     width = 475, height = 600, units = "px")

p
dev.off()


