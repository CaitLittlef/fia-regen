## Predict spatially

par(mfrow=c(1,1))

# Which species?
sp
if (sp == "pipo") rng.r <- pipo.rng else rng.r <- psme.rng
plot(st_geometry(rng.r))
sp

# which explan vars?
pipo.explan.vars # "YEAR.DIFF"     "BALive_brt_m"  "tmax.tc"       "def59_z_max13" " FIRE.SEV"
psme.explan.vars # "YEAR.DIFF"     "BALive_brt_m"  "tmax.tc"       "DUFF_DEPTH_cm"

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
# yr <- 15
YEAR.DIFF[! is.na(YEAR.DIFF)] <- yr
extent(YEAR.DIFF)

BALive_brt_m <- r
BALive_brt_m[! is.na(BALive_brt_m)] <- mean(data.brt$BALive_brt_m)
extent(BALive_brt_m)

DUFF_DEPTH_cm <- r
DUFF_DEPTH_cm[! is.na(DUFF_DEPTH_cm)] <- mean(data.brt$DUFF_DEPTH_cm)
extent(DUFF_DEPTH_cm)

FIRE.SEV <- r
FIRE.SEV[! is.na(FIRE.SEV)] <- Mode(data.brt$FIRE.SEV)
extent(FIRE.SEV)

## For deficit, could use average max
def59_z_max13 <- r
def59_z_max13[! is.na(def59_z_max13)] <- mean(data.brt$def59_z_max13) ; defz <- "avg_max"

## Alternative: use 1 droughty yr or take 3-yr max for periods with clear spatial patterns to CMD

# 2012: generally very droughty across US, especially southern US.
# def59_z_max13 <- raster("C:/Users/clittlef/Google Drive/2RMRS/fia-regen/data/terraclimate/def_z/def.2012.5.9.tif") %>%
#   crop(rng.r) %>%
#   mask(rng.r) %>%
#   extend(rng.r) ; defz <- "2012"# extend b/c for whatever reason shrunk

# 2017: v. droughty in N. Rockies.
# def59_z_max13 <- raster("C:/Users/clittlef/Google Drive/2RMRS/fia-regen/data/terraclimate/def_z/def.2017.5.9.tif") %>%
#   crop(rng.r) %>%
#   mask(rng.r) %>%
#   extend(rng.r) ; defz <- "2017"# extend b/c for whatever reason shrunk

# Max deficit from 2010-2012; 2015-2017
# defz <- "max1012" ; def59_z_max13 <- def1012 %>% # from 02b
defz <- "max1517" ; def59_z_max13 <- def1517 %>% # from 02b
  crop(rng.r) %>%
  mask(rng.r) %>%
  extend(rng.r) 


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
names(def59_z_max13) <- "def59_z_max13"
names(DUFF_DEPTH_cm) <- "DUFF_DEPTH_cm"
names(FIRE.SEV) <- "FIRE.SEV"

## Create brick for prediction (ok to have all b/c each sp will only pull what it needs)
brick <- brick(YEAR.DIFF, BALive_brt_m, tmax.tc, def59_z_max13, DUFF_DEPTH_cm, FIRE.SEV)
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

## Combine all predictors into table. *** REFERENCING i in pred loop ABOVE!! ***
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
min(values.xy$value, na.rm = T) #  0.1656636 2010-2012; 0.1656636 2015-2017 (pipo); 0.2338595 (psme)
max(values.xy$value, na.rm = T) # 0.2376445 2010-2012; 0.2376209 2015-2017 (pipo); 0.4677445 (psme)


# Set gradient limits based on sp
sp
if (sp == "pipo") limits <- c(0.16,0.24) else limits <- c(0.23,0.47)
# if (sp == "pipo") limits <- c(0.19,0.35) else limits <- c(0.23,0.47)
limits
# limits <- c(0.19, 0.23) # for pipo when just using avg defz, not 2012 or 2017.
display.brewer.pal(8, "Dark2")



## Plot into map; create shapefile for just Gulf of California
p <- ggplot() +
  # annotate(geom = 'raster', x = hill.data$x, y = hill.data$y,
           # fill = scales::colour_ramp(c("white", "black"))(hill.data$value),
           # interpolate = TRUE)  +
  geom_raster(data = values.xy, aes(x = x, y = y, fill = value)) +
  geom_sf(data = nonIntWest, color = "#808B96", fill = "white") +
  geom_sf(data = IntWsts, color = "#808B96", fill = NA) + # NA else covers tile with color.
  # ggtitle(paste0(sp," at yr ",yr,", deficit z scores: ",defz)) +
  scale_fill_gradient(name = "Prob. juv.\npresence",
                      # low = "#E8F5F1", high = "#105E47", #palette[1],
                      low = "#FFDE35", high = palette[3], #palette[6] lightneed by 20%
                      # found lighter/darker shades here: https://www.tutorialrepublic.com/html-reference/html-color-picker.php
                      # na.value = NA, # doesn't work.
                      na.value = "#EAECEE", # sets background IntW states pale grey
                      limits = limits) +
  coord_sf(xlim = c(-121, -100), ylim = c(30, 50), expand = FALSE) +
  theme_bw(base_size = 18) +
  theme(panel.grid.major = element_line(color = "#808B96", size = 1), # blend lat/long into background
        panel.border = element_rect(fill = NA, color = "black", size = 0.5),
        panel.background = element_rect(fill = "#808B96"), # irrelevant in this plot b/c pred raster
        axis.title = element_blank(),
        axis.line = element_line(size = 0.5),
        legend.background = element_rect(fill = "white", color = "black", size = 0,5),
        legend.justification=c(0,0), # defines which side oflegend .position coords refer to
        legend.position=c(0,0),
        legend.title = element_text(size=12),
        legend.text=element_text(size=10),
        # plot.margin=unit(c(0.5,1.5,1.5,1.5),"cm"))  # top, right, bottom, left
        plot.margin=unit(c(0.5,1.25,0.5,0.5),"cm"))  # top, right, bottom, left

p

png(paste0(out.dir, sp,"_yr",yr,"z_",defz,"_pred_map_",currentDate,".png"),
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

