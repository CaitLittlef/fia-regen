## Predict spatially

# Which species?
sp
if (sp == "pipo") rng.r <- pipo.rng else rng.r <- psme.rng
plot(st_geometry(rng.r))
sp

# which explan vars?
pipo.explan.vars # "YEAR.DIFF"     "BALive_brt_m"  "tmax.tc"       "def59_z_max15"
psme.explan.vars # "YEAR.DIFF"     "BALive_brt_m"  "tmax.tc"       "DUFF_DEPTH_cm"

## What are vars for which I need rasters? Vary year, let tmax.tc change in space, hold others at mean

# Predict only to range; use a terraclimate raster as template
tmax.tc <- raster(paste0(tc.dir,"tmax.1981.2010.tif"))
tmax.tc <- tmax.tc %>% crop(rng.r) %>% mask(rng.r) # Crop to sp range
r <- tmax.tc 
extent(tmax.tc)

YEAR.DIFF <- r
# how many years?
yr <- 1
yr <- 10
YEAR.DIFF[! is.na(YEAR.DIFF)] <- yr
extent(YEAR.DIFF)

BALive_brt_m <- r
BALive_brt_m[! is.na(BALive_brt_m)] <- mean(data.brt$BALive_brt_m)
extent(BALive_brt_m)


def59_z_max15 <- r
def59_z_max15[! is.na(def59_z_max15)] <- mean(data.brt$def59_z_max15) ; defz <- "avg_max"
# Alternative -- using super drought year values (at least in n rockies)
# def59_z_max15 <- raster("C:/Users/clittlef/Google Drive/2RMRS/fia-regen/data/terraclimate/def_z/def.2017.5.9.tif") %>%
#   crop(rng.r) %>%
#   mask(rng.r) %>%
#   extend(rng.r) ; defz <- "2017"# extend b/c for whatever reason shrunk
# plot(def59_z_max15)
# extent(def59_z_max15)
if (sp == "pipo") defz <- defz else defz <- "na"
defz

DUFF_DEPTH_cm <- r
DUFF_DEPTH_cm[! is.na(DUFF_DEPTH_cm)] <- mean(data.brt$DUFF_DEPTH_cm)
extent(DUFF_DEPTH_cm)

## Yet the underlying names are still remnants of orig raster file
labels(tmax.tc) # gives "tmax.1981.2010"
names(tmax.tc) <- "tmax.tc"
labels(tmax.tc) # gives "tmax.tc"
names(YEAR.DIFF) <- "YEAR.DIFF"
names(BALive_brt_m) <- "BALive_brt_m"
names(def59_z_max15) <- "def59_z_max15"
names(DUFF_DEPTH_cm) <- "DUFF_DEPTH_cm"

# Create brick for prediction (ok to have all b/c each sp will only pull what it needs)
brick <- brick(YEAR.DIFF, BALive_brt_m, tmax.tc, def59_z_max15, DUFF_DEPTH_cm)
names(brick)

# Predict! This currently just uses first model; run for all 100 and take avg.
space <- raster::predict(brick, models[[1]], n.trees=models[[1]]$n.trees, type = "response")
plot(space)


# Turn deficit raster into table (FUNCTION DEFINED AT SET-UP)
space.data <- gplot_data(space)
min(space.data$value, na.rm = T) # 0.1539761 pipo, 0.2455601 psme
max(space.data$value, na.rm = T) # 0.2347101 pipo, 0.4743217 psme

# Set gradient limits based on sp
sp
if (sp == "pipo") limits <- c(0.15,0.24) else limits <- c(0.24,0.48)
limits

## Plot into map
p <- ggplot() +
  geom_tile(data = space.data, aes(x = x, y = y, fill = value)) +
  geom_sf(data = nonIntWest, color = "#808B96", fill = "white") +
  geom_sf(data = IntWsts, color = "#808B96", fill = NA) + # NA else covers tile with color.
  scale_fill_gradient("Probability of juvenile presence",
                      low = palette[6], high = palette[3],
                      na.value = "#EAECEE", # sets background IntW states pale grey
                      limits = limits) +
                      # limits = c(0.15,0.24)) + # pipo, SETS SAME GRADIENT YR 1, YR 10
                      # limits = c(0.24,0.48)) + # psme, SETS SAME GRADIENT YR 1, YR 10
  coord_sf(xlim = c(-121, -100), ylim = c(30, 50), expand = FALSE) +
  ggtitle(paste0(sp," at yr ",yr,", deficit z scores: ",defz)) +
  theme_bw(base_size = 18) +
  theme(panel.grid.major = element_line(color = "#808B96"), # blend lat/long into background
        axis.title = element_blank(),
        legend.title = element_blank(),
        legend.background = element_rect(color = "#808B96"),
        legend.justification=c(0,0), # defines which side oflegend .position coords refer to 
        legend.position=c(0,0),
        legend.text=element_text(size=12),
        plot.title = element_text(size=12))
p
tiff(paste0(out.dir, sp,"_yr",yr,"z_",defz,"_pred_map_",currentDate,".tiff"),
   width = 450, height = 600, units = "px")
p
dev.off()

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

