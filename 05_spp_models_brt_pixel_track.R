## ID pixels on extremes of climatic variability dipole and predict through time.
# Builds right from 02b_terraclim_map.R -- load all of those climate layers

## If 02b has been run, no need to re-run this chunk.
setwd(paste0(tc.dir,"/def_z/"))
def.tifs = list.files(pattern="*5.9.tif", full.names = TRUE)
def.stack <- stack(def.tifs)
wd <- setwd("C:/Users/clittlef/Google Drive/2RMRS/fia-regen/data")
def.stack <- def.stack %>% crop(IntWsts) %>% mask(IntWsts)
names(def.stack) <- paste0("def", right(c(1981:2017),2))



####ID FOCAL PIXELS#####################################################
# Pick 10-12 and 15-17 maxes given big spatial variability
# def9395 <-  overlay(def.stack$def93, def.stack$def94, def.stack$def95,
#                     fun=function(x){ max(x,na.rm=T)})
# def1012 <-  overlay(def.stack$def10, def.stack$def11, def.stack$def12,
#                     fun=function(x){ max(x,na.rm=T)})
def1517 <-  overlay(def.stack$def15, def.stack$def16, def.stack$def17,
                    fun=function(x){ max(x,na.rm=T)})

# 
# ## For trajectory, find points on either extreme (min and max deficits)
# maxValue(def1012) # 5.88
# minValue(def1012)# -Inf
# def1012[def1012 < -50] <- NA
# minValue(def1012) # -0.61
# 
# maxValue(def1517) # 3.21
# minValue(def1517)# -Inf
# def1517[def1517 < -50] <- NA
# minValue(def1517)# -1.41
# 
# ## What's position of those?
# pix.min.1012 <- data.frame(rasterToPoints(def1012,
#                                           function(x) x == minValue(def1012), spatial = FALSE))
# pix.max.1012 <- data.frame(rasterToPoints(def1012,
#                                           function(x) x == maxValue(def1012), spatial = FALSE))
# pix.min.1517 <- data.frame(rasterToPoints(def1517,
#                                           function(x) x == minValue(def1517), spatial = FALSE))
# pix.max.1517 <- data.frame(rasterToPoints(def1517,
#                                           function(x) x == maxValue(def1517), spatial = FALSE))
# 
# # Can add these into map with code like this:
# # geom_point(data = pix.min.1012, aes(x=x, y=y), color = "red") +
# 
# # Save to csv. for future look-up
# pixels <- rbind(pix.min.1012, pix.max.1012, pix.min.1517, pix.max.1517)
# pixels$layer <- NULL
# write.csv(pixels, "loc.pixels.csv", row.names = FALSE)
pixels <- read.csv("loc.pixels.csv")
rownames(pixels) <- c("pix.min.1012",
                      "pix.max.1012",
                      "pix.min.1517",
                      "pix.max.1517")

# What are values to use as predictors associated with each point?
# Use actual spatial climate (tmax) but avgs for other predictors (BA, fire sev)



####DATA FOR FOCAL PIXELS#####################################################
## Iteratively extract 3-yr maxes for each of the selected pixels from '81-'17
# But must end before 2016 at 2015 else can't get 3 yrs inclusive (no 2018)
# Need as.numeric() else named number.
maxes.temp <- vector() # empty vector to drop maxes for each 3yr period
maxes <- matrix(data = NA, nrow = 4, ncol = nbands(def.stack)-2) # empty matrix to fill with maxes
# l <- list()
for(p in 1:nrow(pixels)){
  maxes.temp <- vector()
  for(b in 1:(nbands(def.stack)-2)){
    temp0 <- as.numeric(raster::extract(def.stack[[b]], pixels[p,]))
    temp1 <- as.numeric(raster::extract(def.stack[[b+1]], pixels[p,]))
    temp2 <- as.numeric(raster::extract(def.stack[[b+2]], pixels[p,]))
    max.temp <- max(temp0, temp1, temp2)
    maxes.temp <- c(maxes.temp, max.temp)
  }
  maxes[p,] <- maxes.temp
}


maxes <- data.frame(maxes)
colnames(maxes) <- c(paste0("start_",(1981:2015)))
rownames(maxes) <- c("pos.min.1012", "pos.max.1012", "pos.min.1517", "pos.max.1517")
# For later, save pixel maxes
max3yr.nID <- maxes[4,]
max3yr.nID <-as.data.frame(t(max3yr.nID))
max3yr.nID$year <- as.integer(right(rownames(max3yr.nID), 4))
max3yr.nID <- max3yr.nID %>% rename(value = pos.max.1517)

max3yr.cAZ <- maxes[3,]
max3yr.cAZ <-as.data.frame(t(max3yr.cAZ))
max3yr.cAZ$year <- as.integer(right(rownames(max3yr.cAZ), 4))
max3yr.cAZ <- max3yr.cAZ %>% rename(value = pos.min.1517)

## Grab annual vals too JIC
avg.nID <- as.numeric(raster::extract(def.stack, pixels[4,]))
avg.nID <- data.frame(avg.nID) ; colnames(avg.nID) <- "value"
avg.nID$year <- as.numeric(c(paste0((1981:2017))))

avg.cAZ <- as.numeric(raster::extract(def.stack, pixels[3,]))
avg.cAZ <- data.frame(avg.cAZ) ; colnames(avg.cAZ) <- "value"
avg.cAZ$year <- as.numeric(c(paste0((1981:2017))))


## Get other predictor variables set for using predicting response at these pixels.
pix.preds <- maxes
tmax.tc <- raster(paste0(tc.dir, "tmax.1981.2010.tif"))
tmax.tc <- raster::extract(tmax.tc, pixels)
pix.preds <- cbind(pix.preds, tmax.tc)
pix.preds$YEAR.DIFF <- as.integer(10)
pix.preds$BALive_brt_m <- mean(data.brt$BALive_brt_m)
pix.preds$FIRE.SEV <- Mode(data.brt$FIRE.SEV)
pix.preds <- pix.preds %>% dplyr::select(BALive_brt_m, YEAR.DIFF, tmax.tc, FIRE.SEV, everything())





####PIXEL PREDICTIONS#####################################################
## Predict each pixel's likelihood of juv presence for each 3yr interval, by 100 models.
preds.all.defs <- NULL # 35 vals (for each def)
preds.all.mods <- NULL # 100 rows (for each model), 35 cols (for each def)
preds.all.pix <- list() # there will be 4 (for each pixel) of 100x35
for(p in 1:4){ # for each of 4 pixels
  # for(i in 1:5) { # for each of 5 models
  for(i in 1:length(models)) { # for each of 100 models
    for(d in 1:length(maxes)){# for each of the 3-yr deficit values
      newdata = pix.preds[p,c(1:4,4+d)] # keep yr, ba, tmax, sev & 1 def
      colnames(newdata)[5] <- "def59_z_max13" # rename def to what mod expects
      temp <- predict.gbm(models[[i]],
                          newdata=newdata,
                          n.trees = models[[i]]$n.trees,
                          type = "response")
      preds.all.defs <- cbind(preds.all.defs,
                              temp) # bind preds for each 3 yr stretch
      temp <- NULL
    }
    preds.all.mods <- rbind(preds.all.mods,
                            preds.all.defs) # stick that models predictions into a row
    preds.all.defs <- NULL
  }
  preds.all.pix[[p]] <- preds.all.mods # Stick df of preds into list
  preds.all.mods <- NULL 
}

## Take mean of each column (diff 3yr starts) aross all mods
# Apply colMean to each list element (df); return list
preds <- lapply(preds.all.pix, colMeans) 
preds <- data.frame(do.call(rbind, preds)) # make df else can't gather below.
colnames(preds) <- (1981:2015)
preds$pixel <- rownames(pixels) # Alt: set rownames then rownames_to_columns


## Gather into long-form with diff rows for each pixel x year; don't gather pixel
pred.long <- tidyr::gather(preds, key = "year", value = "prob", -pixel)
pred.long$year <- as.numeric(pred.long$year)
# Drop anything before 1984 (earliest fire yr) and after 2015 (last inventory)
pred.long <- pred.long %>% filter(year > 1983 & year < 2016)

## Chose with pixel pairs -- those which were extremely diff in...
# 2010-2012
# pred.long <- pred.long %>%
#   filter(pixel == c("pix.min.1012", "pix.max.1012")) ;
# pix.names <- c("CO/NM border", "southwest UT")
# pix <- c("CO_UT")

# 2015-2017
pred.long <- pred.long %>%
  filter(pixel == c("pix.min.1517", "pix.max.1517")) ;
pix.names <- c("northern ID", "central AZ")
pix <- c("ID_AZ")


## How smooth should the line be?
# span <- 0.1
span <- 0.2
# span <- 0.3
# span <- 0.4
# span <- 0.5

## Plot likelihood of pixels thru time
# Try to smooth line w/ polynomial; can't go over 24 degrees:
# Computation failed in `stat_smooth()`:
#   'degree' must be less than number of unique points 
# Even jittering values slightly (as some are the same) doesn't change
# pred.long$prob <- jitter(pred.long$prob, 2)
p1 <- ggplot(pred.long, aes(x = year, y = prob, group = pixel, color = pixel)) +
  # geom_point() +
  geom_line() +
  # geom_smooth(method = "lm", formula = y ~ poly(x, 12), se = FALSE) +
  # geom_smooth(method = "loess", span = span, se = FALSE) +
  scale_color_manual(values = palette[4:5],
  labels = pix.names) +
  labs(x = NULL,
       y = "Probability of juvenile presence") +
  scale_x_continuous(breaks = seq(1980, 2015, 5)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.justification=c(1,0), 
        legend.position = c(1, 0.33),
        # legend.position = c(0.1, 0.75),
        # legend.position = "bottom",
        legend.title = element_blank(),
        legend.box = "horizontal",
        legend.text=element_text(size=12), #angle = 90),
        legend.background = element_rect(color = "transparent", fill = "transparent")) 
dev.off()
p1



# Save
tiff(paste0(out.dir, "pixel_track_",pix,"_loess",span,".tiff"), width = 600, height = 200)
p1
dev.off()



####ALTERNATIVE TO INDIVID PIXELS: ZONES#####################################################
## N.b., I could use level III ecoregions, but in AZ at least, level III spans areas w/ big and sml deficit
## Load AZ & ID level IV ecoregions; fix any invalid geometries
# AZ4 <- st_read(dsn = "ecoregIV/az_eco_l4.shp") %>%  st_buffer(dist = 0) 
# ID4 <- st_read(dsn = "ecoregIV/id_eco_l4.shp") %>%  st_buffer(dist = 0)





####ENSO/PDO INDICES#####################################################
## To match 3-year deficit max, chose thru 2015 (<2016)

## MEI
# Source: https://www.esrl.noaa.gov/psd/enso/mei/
mei <- read.csv("meiv2.data.csv")
mei <- mei %>%
  mutate(dec.lag = lag(dec, 1)) # put prior december into next "growing" year
mei %>% dplyr::select(dec, dec.lag) # checks out
mei.winter <- mei %>%
  group_by(year) %>%
  summarise(value = mean(dec.lag, jan, feb))
# Keep only years that correspond w/ FIA predictions
# 1st fire yr is 1984, last inventory yr is 2015
mei.winter <- mei.winter %>%
  # filter(year > 1983 & year <2016) 
  filter(year > 1980 & year <2018) 


## NINO34
# Source: https://climatedataguide.ucar.edu/climate-data/nino-sst-indices-nino-12-3-34-4-oni-and-tni
nino34 <- read.csv("nina34.data.csv")
head(nino34)
nino34 <- nino34 %>%
  mutate(dec.lag = lag(dec, 1)) # put prior december into next "growing" year
nino34 %>% dplyr::select(dec, dec.lag) # checks out
nino34.winter <- nino34 %>%
  group_by(year) %>%
  summarise(value = mean(dec.lag, jan, feb))
# Keep only years that correspond w/ FIA predictions
# 1st fire yr is 1984, last inventory yr is 2015
nino34.winter <- nino34.winter %>%
  # filter(year > 1983 & year <2016) 
  filter(year > 1980 & year <2018) 


## PDO
# Source: https://www.ncdc.noaa.gov/teleconnections/pdo/
# Ref: https://climatedataguide.ucar.edu/climate-data/north-pacific-np-index-trenberth-and-hurrell-monthly-and-winter
pdo <- read.csv("pdo.csv")
pdo <- rownames_to_column(pdo)
colnames(pdo) <- c("date", "value")
pdo <-pdo[-(1),]
pdo$date <- as.numeric(pdo$date)
pdo$value <- as.numeric(pdo$value)
# plot(pdo$value ~ pdo$date)
pdo$year <- as.integer(left(pdo$date, 4))
pdo$month <- right(pdo$date, 2)
pdo$date <- NULL
pdo <- spread(pdo, key = month, value = value)
colnames(pdo) <- c("year", "jan", "feb", "march", "april", "may", "june",
                  "july", "aug", "sept", "oct", "nov", "dec")
pdo <- pdo %>%
  mutate(nov.lag = lag(nov, 1),
         dec.lag = lag(dec, 1))
pdo %>% dplyr::select(nov, nov.lag, dec, dec.lag)
pdo.winter <- pdo %>%
  group_by(year) %>%
  summarise(value = mean(nov.lag, dec.lag, jan, feb, march)) # based on ref above
pdo.winter <- pdo.winter %>%
  # filter(year > 1983 & year <2016) 
  filter(year > 1980 & year <2018) 



## PC1
# Source: Jon Abatzoglou generated; captures N-S dipole.
# Positive values: HIGH deficit (dry) in SW; LOW deficit (moist) in N. Rockies
# Negative values: LOW deficit (moist) in SW; HIGH deficit (dry) in N. Rockies
pc1.raw <- read.table("pc1.txt", header = TRUE, sep=",")
pc1.raw <- pc1.raw %>% rename(value = score1)
pc1.raw <- pc1.raw %>% filter(year > 1983 & year <2018) 
# Don't filter to 2015 yet; wait til moving windox max extracted.

# Find 3-yr max and min w/ moving window.
library(RcppRoll)
# Rolling has 2 fewer yrs than raw b/c stops at 3rd to last.
# Retain through 2015, corresponding to row 32 in raw.
temp.max <- roll_max(pc1.raw$value, n = 3, align = "left")
pc1.max3yr <- as.data.frame(cbind(pc1.raw$year[1:32], temp.max))
colnames(pc1.max3yr) <- c("year", "value")

temp.min <- roll_min(pc1.raw$value, n = 3, align = "left")
pc1.min3yr <- as.data.frame(cbind(pc1.raw$year[1:32], temp.min))
colnames(pc1.min3yr) <- c("year", "value")

temp.mean <- roll_mean(pc1.raw$value, n = 3, align = "left")
pc1.mean3yr <- as.data.frame(cbind(pc1.raw$year[1:32], temp.mean))
colnames(pc1.mean3yr) <- c("year", "value")


# Now fully filter pc1.raw
pc1.raw <- pc1.raw %>% filter(year > 1983 & year <2016) 
# install.packages("RcppRoll")



## pick which dataset to plot; set to p2, p3, then p4
# mode.data <- mei.winter ; ylab <- "mei"
# mode.data <- nino34.winter ; ylab <- "nino34"
# mode.data <- pdo.winter ; ylab <- "pdo"

mode.data <- pc1.raw ; ylab <- "pc1" ; mode.data <- mode.data %>% filter(year > 1983)
mode.data <- pc1.max3yr ; ylab <- "pc1 3yr max" ; mode.data <- mode.data %>% filter(year > 1983)
mode.data <- pc1.min3yr ; ylab <- "pc1 3yr min" ; mode.data <- mode.data %>% filter(year > 1983)
mode.data <- pc1.mean3yr ; ylab <- "pc1 3yr mean" ; mode.data <- mode.data %>% filter(year > 1983)

mode.data <- avg.nID ; ylab <- "avg n ID" ; mode.data <- mode.data %>% filter(year > 1983)
mode.data <- max3yr.nID ; ylab <- "3yrmax n ID" ; mode.data <- mode.data %>% filter(year > 1983)
mode.data <- avg.cAZ ; ylab <- "avg c AZ" ; mode.data <- mode.data %>% filter(year > 1983)
mode.data <- max3yr.cAZ ; ylab <- "3yrmax c AZ" ; mode.data <- mode.data %>% filter(year > 1983)


# Plot climate mode (change p# iteratively when selecting index)
p5 <- ggplot(mode.data, aes(x = year, y = value)) +
  geom_line(aes(x = year, y = value)) +
  ylab(paste0(ylab)) + 
  scale_x_continuous(breaks = seq(1980, 2015, 5)) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
p3 

# MEI & NINO34 look very similar



####JOINT PLOTS#####################################################

## To stack plots with aligned width, extract max width from each object.
# Ref: https://stackoverflow.com/questions/36198451/specify-widths-and-heights-of-plots-with-grid-arrange

plots <- list(p1, p2)
plots <- list(p1, p2, p3)
plots <- list(p1, p2, p3, p4)
plots <- list(p1, p2, p3, p4, p5)
plots <- list(p1, p2, p3, p4, p5, p6)

grobs <- list()
widths <- list()

## Collect the widths for each grob of each plot
for (l in 1:length(plots)){
  grobs[[l]] <- ggplotGrob(plots[[l]])
  widths[[l]] <- grobs[[l]]$widths[2:5]
}

## Use do.call to get the max width
maxwidth <- do.call(grid::unit.pmax, widths)

## Assign the max width to each grob
for (l in 1:length(grobs)){
  grobs[[l]]$widths[2:5] <- as.list(maxwidth)
}

## Plot
# tiff(paste0(plot.dir, explan.vars[[i]], ".tiff"))
# do.call("grid.arrange", c(grobs, ncol = 1))
# grid.arrange(grobs = grobs, ncol = 1, heights = c(1, 1))
grid.arrange(grobs = grobs, nrow = 6, ncol = 1, heights = c(1, 1, 1, 1, 1, 1))
grid.arrange(grobs = grobs, nrow = 5, ncol = 1, heights = c(1, 1, 1, 1, 1))
grid.arrange(grobs = grobs, nrow = 4, ncol = 1, heights = c(1, 1, 1, 1))
grid.arrange(grobs = grobs, nrow = 3, ncol = 1, heights = c(1, 1, 1))
tiff(paste0(out.dir, "cmd anomaly nID vs indices.tiff"))
dev.off()

