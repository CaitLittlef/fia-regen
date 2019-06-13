data.pipo <- read.csv("data.pipo_2019-05-14.csv") ; data.pipo$X <- NULL
data.psme <- read.csv("data.psme_2019-05-14.csv") ; data.psme$X <- NULL

## Convert BA to metric
data.pipo$BALiveTot_m <- data.pipo$BALiveTot/4.359
data.pipo$BALive_pipo_m <- data.pipo$BALive_pipo/4.359

# Check
data.pipo$BALiveTot_m[1:5] ; data.pipo$BALiveTot[1:5]
data.pipo$BALive_pipo_m[1:5] ; data.pipo$BALive_pipo[1:5]
par(mfrow=c(1,2))
hist(data.pipo$BALiveTot, breaks = 10) ; hist(data.pipo$BALiveTot_m, breaks = 10)
par(mfrow=c(1,1))


data.psme$BALiveTot_m <- data.psme$BALiveTot/4.359
data.psme$BALive_psme_m <- data.psme$BALive_psme/4.359

## Convert litter/duff to metric
data.pipo$DUFF_DEPTH_cm <- data.pipo$DUFF_DEPTH * 2.538
data.pipo$LITTER_DEPTH_cm <- data.pipo$LITTER_DEPTH * 2.538

# Check
data.pipo$DUFF_DEPTH_cm[1:100] ; data.pipo$DUFF_DEPTH[1:100]
data.pipo$LITTER_DEPTH_c[1:100] ; data.pipo$LITTER_DEPTH[1:100]
par(mfrow=c(1,2))
hist(data.pipo$DUFF_DEPTH, breaks = 10) ; hist(data.pipo$DUFF_DEPTH_cm, breaks = 10)
par(mfrow=c(1,1))

data.psme$DUFF_DEPTH_cm <- data.psme$DUFF_DEPTH * 2.538
data.psme$LITTER_DEPTH_cm <- data.psme$LITTER_DEPTH * 2.538



# Other vars
# Create CMD relative chng (from observed z-score slopes)
data.pipo$CMD_CHNG <- data.pipo$def59.z.slope
data.psme$CMD_CHNG <- data.psme$def59.z.slope

# Create proportion BA
data.pipo$BAProp_pipo <- data.pipo$BALive_pipo/data.pipo$BALiveTot
data.psme$BAProp_psme <- data.psme$BALive_psme/data.psme$BALiveTot


currentDate <- Sys.Date()
write.csv(data.pipo, paste0("data.pipo_", currentDate,".csv"))
write.csv(data.psme, paste0("data.psme_", currentDate,".csv"))
