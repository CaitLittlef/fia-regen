## ID pixels on extremes of climatic variability dipole and predict through time.
# Builds right from 02b_terraclim_map.R -- load all of those climate layers

## Pasted from 02b (where code is commented)
setwd(paste0(tc.dir,"/def_z/"))
def.tifs = list.files(pattern="*5.9.tif", full.names = TRUE)
def.stack <- stack(def.tifs)
wd <- setwd("C:/Users/clittlef/Google Drive/2RMRS/fia-regen/data") # If working with/within drive
def.stack <- def.stack %>% crop(IntWsts) %>% mask(IntWsts)
names(def.stack) <- paste0("def", right(c(1981:2017),2))

# # Pick 10-12 and 15-17 maxes given big spatial variability
# def9395 <-  overlay(def.stack$def93, def.stack$def94, def.stack$def95,
#                     fun=function(x){ max(x,na.rm=T)})
# def1012 <-  overlay(def.stack$def10, def.stack$def11, def.stack$def12,
#                     fun=function(x){ max(x,na.rm=T)})
# def1517 <-  overlay(def.stack$def15, def.stack$def16, def.stack$def17,
#                     fun=function(x){ max(x,na.rm=T)})
# plot(def9395)
# plot(def1012)
# plot(def1517)
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
# # pixels <- rbind(pix.min.1012, pix.max.1012, pix.min.1517, pix.max.1517)
# # pixels$layer <- NULL
# # write.csv(pixels, "pred.pixels.csv", row.names = FALSE)
pixels <- read.csv("pred.pixels.csv")

# What are values to use as predictors associated with each point?
# Retain spatial climate (tmax) but impute avgs. for other predictors
# will need to get 3-yr def max for each 3-yr period since begin of record.

## Iteratively extract 3-yr maxes for each of the selected pixels from '81-'17
# But must end before 2016 else can't get 3 yrs inclusive (no 2018)
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


## Get other predictor variables set for using predicting response at these pixels.
pix.preds <- maxes
tmax.tc <- raster(paste0(tc.dir, "tmax.1981.2010.tif"))
tmax.tc <- raster::extract(tmax.tc, pixels)
pix.preds <- cbind(pix.preds, tmax.tc)
pix.preds$YEAR.DIFF <- as.integer(10)
pix.preds$BALive_brt_m <- mean(data.brt$BALive_brt_m)
pix.preds$FIRE.SEV <- Mode(data.brt$FIRE.SEV)
pix.preds <- pix.preds %>% dplyr::select(BALive_brt_m, YEAR.DIFF, tmax.tc, FIRE.SEV, everything())


## I don't think adding in these additional vars should matter, but maybe...
# pix.preds$def.tc <- mean(data.brt$def.tc)
# pix.preds$ppt.tc <- mean(data.brt$ppt.tc)
# pix.preds$DUFF_DEPTH_cm <- mean(data.brt$DUFF_DEPTH_cm)
# pix.preds$LITTER_DEPTH_cm <- mean(data.brt$LITTER_DEPTH_cm)
# pix.preds$REBURN <- Mode(data.brt$REBURN)
# pix.preds <- pix.preds %>% dplyr::select(BALive_brt_m, YEAR.DIFF, tmax.tc, FIRE.SEV,
#                                          def.tc, ppt.tc, DUFF_DEPTH_cm, LITTER_DEPTH_cm, REBURN,
#                                          everything())

head(pix.preds, 2)

# Fill prediction table: each row is diff model, each column diff 3yr def
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
      preds.all.defs <- cbind(preds.all.defs, temp) # bind preds for each 3 yr stretch
      temp <- NULL
    }
    preds.all.mods <- rbind(preds.all.mods, preds.all.defs) # stick that models predictions into a row
    preds.all.defs <- NULL
  }
  preds.all.pix[[p]] <- preds.all.mods # Stick that pixel's predictions into a list of predictions
  preds.all.mods <- NULL 
}


view(preds.all.pix[[4]])





warnings()

a <- colnames(data.brt)[7]
b <- colnames(newdata)[5]
identical(a, b)

colnames(newdata)
colnames(data.brt)
d
summary(models[[2]])

pred.list[[p]] <- preds
lapply to average all the means within the 100 df per pixel

start <- Sys.time() 

preds <- list()
for (i in 1:5) {
  # for (i in 1:100) {
  preds[[i]] <- brick %>% 
    gbm::predict.gbm(models[[i]],
                     newdata = 
                     n.trees = models[[i]]$n.trees,
                     type = "response") %>%
    gplot_data()
}
print(Sys.time() - start)

?predict.gbm
n.trees = models[[i]]$n.trees,
maxes
