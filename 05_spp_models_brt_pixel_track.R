## ID pixels on extremes of climatic variability dipole and predict through time.
# Builds right from 02b_terraclim_map.R -- load all of those climate layers

## For trajectory, find points on either extreme (min and max deficits)
maxValue(def1012) # 5.88
minValue(def1012)# -Inf
def1012[def1012 < -50] <- NA
minValue(def1012) # -0.61

maxValue(def1517) # 3.21
minValue(def1517)# -Inf
def1517[def1517 < -50] <- NA
minValue(def1517)# -1.41

# What's position of those?
pos.min.1012 <- data.frame(rasterToPoints(def1012, function(x) x == minValue(def1012), spatial = FALSE))
pos.max.1012 <- data.frame(rasterToPoints(def1012, function(x) x == maxValue(def1012), spatial = FALSE))
pos.min.1517 <- data.frame(rasterToPoints(def1517, function(x) x == minValue(def1517), spatial = FALSE))
pos.max.1517 <- data.frame(rasterToPoints(def1517, function(x) x == maxValue(def1517), spatial = FALSE))

# Can add these into map with code like this:
# geom_point(data = pos.min.1012, aes(x=x, y=y), color = "red") +

# Save to csv. for future look-up
pixels <- rbind(pos.min.1012, pos.max.1012, pos.min.1517, pos.max.1517)
# pixels$layer <- NULL
# write.csv(pixels, "pred.pixels.csv", row.names = FALSE)
pixels <- read.csv("pred.pixels.csv")

# What are values to use as predictors associated with each point?
# Retain spatial climate (tmax) but impute avgs. for other predictors
# will need to get 3-yr def max for each 3-yr period since begin of record.

pix.tmax <- raster::extract(tmax, pixels)
YEAR.DIFF <- 10
BALive_brt_m <- mean(data.brt$BALive_brt_m)
FIRE.SEV <- Mode(data.brt$FIRE.SEV)

# Determine bounds for predictions: first fire yr and final inventory yr
mtbs <- read.csv('SP_MTBS_CMBJoin.csv') ; min(mtbs$Year[mtbs$Year>0])
max(data.pipo$INVYR)







# Predict trajectories for each of those pixels and for each 3-yr time period
predictors<-list(rep(NA,length(models))) ## space for data: however many models are run
responses<-list(rep(NA,length(models)))

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