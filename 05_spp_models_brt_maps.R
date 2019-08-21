## Predict spatially

# Which species?
sp
if (sp == "pipo") rng.r <- pipo.rng else rng.r <- psme.rng
plot(st_geometry(rng.r))
sp

# which explan vars?
pipo.explan.vars # "YEAR.DIFF"     "BALive_brt_m"  "tmax.tc"       "def59_z_max15"
psme.explan.vars # "YEAR.DIFF"     "BALive_brt_m"  "tmax.tc"       "LITTER_DEPTH_cm"

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
YEAR.DIFF[! is.na(YEAR.DIFF)] <- yr
extent(YEAR.DIFF)

BALive_brt_m <- r
BALive_brt_m[! is.na(BALive_brt_m)] <- mean(data.brt$BALive_brt_m)
extent(BALive_brt_m)

LITTER_DEPTH_cm <- r
LITTER_DEPTH_cm[! is.na(LITTER_DEPTH_cm)] <- mean(data.brt$LITTER_DEPTH_cm)
extent(LITTER_DEPTH_cm)

def59_z_max15 <- r
def59_z_max15[! is.na(def59_z_max15)] <- mean(data.brt$def59_z_max15) ; defz <- "avg_max"

## Alternative -- using super drought year

# 2012: generally very droughty across US, especially southern US.
def59_z_max15 <- raster("C:/Users/clittlef/Google Drive/2RMRS/fia-regen/data/terraclimate/def_z/def.2012.5.9.tif") %>%
  crop(rng.r) %>%
  mask(rng.r) %>%
  extend(rng.r) ; defz <- "2012"# extend b/c for whatever reason shrunk

# 2017: v. droughty in N. Rockies.
def59_z_max15 <- raster("C:/Users/clittlef/Google Drive/2RMRS/fia-regen/data/terraclimate/def_z/def.2017.5.9.tif") %>%
  crop(rng.r) %>%
  mask(rng.r) %>%
  extend(rng.r) ; defz <- "2017"# extend b/c for whatever reason shrunk


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
names(def59_z_max15) <- "def59_z_max15"
names(LITTER_DEPTH_cm) <- "LITTER_DEPTH_cm"


## Create brick for prediction (ok to have all b/c each sp will only pull what it needs)
brick <- brick(YEAR.DIFF, BALive_brt_m, tmax.tc, def59_z_max15, LITTER_DEPTH_cm)
names(brick)

## Predict! Below JUST uses first model; instead, run for all 100 and take avg.
# space <- raster::predict(brick, models[[1]], n.trees=models[[1]]$n.trees, type = "response")
# ^ could average all rasters, but I'm guessing that will take forevs.
# How many pixels are we talking?
# length(!is.na(space@data@values)) # gives 436433, which I thing is everything...

# Loop thru to fill list with predictions for each pixel from ALL models
start <- Sys.time() 

preds <- list()
# for (i in 1:5) {
for (i in 1:100) {
  preds[[i]] <- brick %>% 
  raster::predict(models[[i]],
                  n.trees = models[[i]]$n.trees,
                  type = "response") %>%
  gplot_data()
}
print(Sys.time() - start)

## Combine all predictors into table. *** REFERENCING i ABOVE!! ***
# dplyr's bind_rows() avoids duped names unlike do.call(rbind...)
preds.df <- bind_rows(preds) %>% dplyr::select(-variable) # 3 vars
# preds.raw.df <- bind_cols(preds) # 400 vars
nrow(preds.df)/i # 49815 for pipo, 49920 for psme
x <- preds.df$x[1:49815] # for pipo
y <- preds.df$y[1:49815] # for pipo
# x <- preds.df$x[1:49920] # for psme
# y <- preds.df$y[1:49920] # for psme
values.df <- preds.df %>% dplyr::select(-x, -y) 
i
values.df <- as.data.frame(matrix(values.df$value, ncol = i, byrow = FALSE))

# Average all values together
values.df <- values.df %>%
  mutate(value = rowMeans(.)) %>%
  dplyr::select(value)

values.xy <- as.data.frame(cbind(x, y, values.df))
min(values.xy$value, na.rm = T) # 0.1594297 2012 & 2017 <- pipo; 0.2410127 psme
max(values.xy$value, na.rm = T) # 0.2283218 2012, 0.2283177 2017; 0.5131914 psme

# Turn deficit raster into table (FUNCTION DEFINED AT SET-UP)
# space.data <- gplot_data(space)
# min(space.data$value, na.rm = T) # 0.1539761 pipo, 0.2455601 psme
# max(space.data$value, na.rm = T) # 0.2347101 pipo, 0.4743217 psme

# Set gradient limits based on sp
sp
if (sp == "pipo") limits <- c(0.15,0.23) else limits <- c(0.24,0.52)
limits
# limits <- c(0.19, 0.23) # for pipo when just using avg defz, not 2012 or 2017.

## Plot into map; create shapefile for just Gulf of California
p <- ggplot() +
  geom_tile(data = values.xy, aes(x = x, y = y, fill = value)) +
  geom_sf(data = nonIntWest, color = "#808B96", fill = "white") +
  geom_sf(data = IntWsts, color = "#808B96", fill = NA) + # NA else covers tile with color.
  # ggtitle(paste0(sp," at yr ",yr,", deficit z scores: ",defz)) +
  scale_fill_gradient(name = "Prob. juv.\npresence",
                      low = palette[6], high = palette[3],
                      na.value = "#EAECEE", # sets background IntW states pale grey
                      limits = limits) +
  coord_sf(xlim = c(-121, -100), ylim = c(30, 50), expand = FALSE) +
  theme_bw(base_size = 18) +
  theme(panel.grid.major = element_line(color = "#808B96", size = 1), # blend lat/long into background
        panel.border = element_rect(fill = NA, color = "black", size = 0.5),
        axis.title = element_blank(),
        axis.line = element_line(size = 0.5),
        legend.background = element_rect(fill = "white", color = "black", size = 0,5),
        legend.justification=c(0,0), # defines which side oflegend .position coords refer to
        legend.position=c(0,0),
        legend.title = element_text(size=12),
        legend.text=element_text(size=10),
        plot.margin=unit(c(0.5,1.5,1.5,1.5),"cm")) + # top, right, bottom, left
  # annotate("text", x = -120.5, y = 49.5, label = "a) ponderosa pine", hjust = 0)
  annotate("text", x = -120.5, y = 49.5, label = "b) Douglas-fir", hjust = 0)
  # annotate("text", x = -120.5, y = 49.5, label = "b) 2012", hjust = 0)
  # annotate("text", x = -120.5, y = 49.5, label = "d) 2017", hjust = 0)
p
tiff(paste0(out.dir, sp,"_yr",yr,"z_",defz,"_pred_map_",currentDate,".tiff"),
   width = 475, height = 600, units = "px")
p
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

