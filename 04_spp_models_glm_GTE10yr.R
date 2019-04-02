## What causes some places to fail in the long run versus recover?

currentDate <- Sys.Date()

## Load data; remove extraneous column if necessary
data.pipo <- read.csv("data.pipo.csv") ; data.pipo$X <- NULL
# identical(data.pipo$FIRE.YR.x, data.pipo$FIRE.YR.y)
data.pipo$FIRE.YR <- data.pipo$FIRE.YR.x
data.pipo$FIRE.YR.x <- NULL ; data.pipo$FIRE.YR.y <- NULL

data.psme <- read.csv("data.psme.csv") ; data.psme$X <- NULL
# identical(data.psme$FIRE.YR.x, data.psme$FIRE.YR.y)
data.psme$FIRE.YR <- data.psme$FIRE.YR.x
data.psme$FIRE.YR.x <- NULL ; data.psme$FIRE.YR.y <- NULL


## Exclude sites w/o fire OR w/ fire.sev 5 & 6 (here NA)
data.pipo <- data.pipo[! is.na(data.pipo$FIRE.SEV) ,]
data.psme <- data.psme[! is.na(data.psme$FIRE.SEV) ,]

## Correct class of variables
# data.pipo$regen_pipo <- factor(data.pipo$regen_pipo, ordered = FALSE)
data.pipo$regen_pipo <- as.numeric(as.character(data.pipo$regen_pipo)) 
# data.psme$regen_psme <- factor(data.psme$regen_psme, ordered = FALSE)
data.psme$regen_psme <- as.numeric(as.character(data.psme$regen_psme)) 
# data.pipo$FIRE.SEV <- factor(data.pipo$FIRE.SEV, ordered = TRUE)
# data.psme$FIRE.SEV <- factor(data.psme$FIRE.SEV, ordered = TRUE)
data.pipo$FIRE.SEV <- as.numeric(data.pipo$FIRE.SEV)
data.psme$FIRE.SEV <- as.numeric(data.psme$FIRE.SEV)

## Create CMD_CHNG from observed trend in z-scores
data.pipo$CMD_CHNG <- data.pipo$def59.z.slope
data.psme$CMD_CHNG <- data.psme$def59.z.slope




######################################## POINT OF NO RETURN RE: RECOVERY? PIPO
## First, does cumulative likelihood plateau at some point? Pt of no return...?
# Plot partial dependence plots to see relationship btwn YEAR.DIFF & regen.
# Can't use glm.trees w/ pdp::partial (yet). Use plotmo (uses median, vs. pdps use avg)
# http://www.milbo.org/doc/plotmo-notes.pdf

# N.b., see CV response re: pdps with logistic regression
# https://stats.stackexchange.com/questions/394762/partial-dependence-plot-for-glm-in-r-why-linear#394764
# ACTUALLY, run those randomly generated numbers a bunch and you'll eventually get a few curves.
# So, the pdps I'm getting for glm.trees below ARE legit.
# I can force a curve below by looking only at a certain range of BALive_pipo (120-170), for example.
# data <- data.pipo %>%
#   filter(BALive_pipo > 120 & BALive_pipo < 170)
# glm.pipo = glm(regen_pipo ~ YEAR.DIFF + BALive_pipo + FIRE.SEV, data = data, family = "binomial")
# plotmo(glm.pipo, type = "response") # gives probabilities; not always straight whereas log odds stright.


# Orig glm tree with partitions
plotmo(glm.tree.pipo, type = "response") # gives probabilities, not log-odds

# Simple glm tree without partitions
tree <- glmtree(regen_pipo ~ YEAR.DIFF, data = data.pipo, family = "binomial", minsplit = 50, ordinal = "L2")
plotmo(tree, type = "response") # gives probabilities; plotmo sets other vars (if there) to median.
plotmo(tree, type = "response", pmethod = "partdep") # gives probabilities; pdps use avg.
plotmo(tree, type = "link", pmethod = "partdep") # gives log-odds; pdps use avg.



# Something happens around 8 years



######################################## JUST LOOK AT GTE 10 YRS POST-FIRE. PIPO
## Keep sites visited 10 or more years post-fire & gurantee some seed source 
# After model selected, scale variables to be able to compare
data.pipo$regen_pipo <- as.numeric(as.character(data.pipo$regen_pipo))
moo <- data.pipo[data.pipo$YEAR.DIFF > 7 & data.pipo$BALive_pipo > 0,]
moo$BAProp_pipo <- moo$BALive_pipo/moo$BALiveTot

## Is there a pattern to failure?
# p <- ggplot() +
#   geom_sf(data = Wsts) +
#   geom_point(data = moo,
#              aes(x = LON_FS, y = LAT_FS, col = regen_pipo)) +
#   scale_color_manual(values = c("blue", "yellow"),
#                      labels = c("no regen", "regen"))
# p # Maybe somethign in sky islands? Particular fire...?


## What predicts regen failure in the long-run?
mod.pipo = glm(regen_pipo ~ BALive_pipo + ppt.tc + tmax.tc + def.tc + aet.tc + def59_z_3 + FIRE.SEV,
               data = moo, family = binomial(link = "logit"))
summary(mod.pipo)  
plotmo(mod.pipo)

## Partial dependence plots -- note you see a much greater threshold with rf than glm.
# moo$regen_pipo <- factor(moo$regen_pipo, levels= c("1", "0"), ordered = FALSE) ; levels(moo$regen_pipo)[1]
# rf <- randomForest(regen_pipo ~ BALive_pipo + ppt.tc + tmax.tc + def.tc + aet.tc + def59_z_3 + FIRE.SEV,
#                    data = moo, importance = TRUE)
# pdp::partial(rf, pred.var = "def59_z_3",
#              plot = TRUE, rug = TRUE, prob = TRUE)
# # ^ As deficit increases, you're less likely to get level 1 of this factor -- i.e., regen.
# 
# moo$regen_pipo <- as.numeric(as.character(moo$regen_pipo))
# rf <- randomForest(regen_pipo  ~ BALive_pipo + MAP_1995 + CMD_1995 + def59_z_13 + FIRE.SEV,
#                    data = moo, importance = TRUE)
# pdp::partial(rf, pred.var = "def59_z_13",
#              plot = TRUE, rug = TRUE, prob = TRUE)
# # ^ As deficit increases, you're less likely to see out outcome of 1 -- i.e., 1 regen (numeric)

######################################
## Eval model
# Standard plots
plot(mod.pipo)

## Deviance over null
mod.pipo.null <- update(mod.pipo, regen_pipo ~ 1)
(deviance(mod.pipo.null) - deviance(mod.pipo))/deviance(mod.pipo.null) 


# ROC
regen_pred=predict(mod.pipo, type ="response")
moo$regen_pred <- regen_pred
roccurve <- roc(regen_pipo ~ regen_pred, data = moo)
par(mfrow=c(1,1))
plot(roccurve)
auc(roccurve)

## 5-fold AUC

# Randomly shuffle data
moo<-moo[sample(nrow(moo)),]
# Create 10 equally size folds
folds <- cut(seq(1,nrow(moo)),breaks=10,labels=FALSE)

AUC <- NULL
for(i in 1:5){
  #Segement data by fold 
  foldIndexes <- which(folds==i,arr.ind=TRUE)
  foldData <- moo[foldIndexes, ]

  # Run model with this fold
  mod <- update(mod.pipo, data = foldData)
  
  # Predict with that fold
  foldPred <- predict(mod, foldData, type = "response")
  
  # Generate ROC
  roccurve <- roc(regen_pipo ~ foldPred, data = foldData)
  
  # Get areaunder the curve
  area <- auc(roccurve)
  
  # Fill AUC matrix with areas
  AUC <- cbind(AUC, area)
}
(AUC.5fld <- rowMeans(AUC))
# ^ Percentage correctly classified obs of all obs.
rm(roccurve, area, AUC)




# Prediction rate on orig data (confusion matrix) 
regen_pred <- predict(mod.pipo, type = "response")
thresh <- 0.5 # define threshold for categorizing predicted probabilities.
# Then cut predicted probabilities into categories 
predCat <- cut(regen_pred, breaks=c(-Inf, thresh, Inf), labels=c("0", "1"))
# Create confusion matrix
confusn <- table(predCat, moo$regen_pipo, dnn=c("predicted", "actual"))
addmargins(confusn)


# Alt: caret package
regen_pred <- predict(mod.pipo, type = "response")
confusionMatrix(data = as.factor(as.numeric(regen_pred>0.5)), # needs factor but set num for 0/1 first
                reference = as.factor(moo$regen_pipo), # needs factor
                dnn = c("predicted", "actual"))

# CV
cv.glm(moo, mod.pipo, K=10)$delta # 10-fold cross-validation: raw & adjusted; nb no K sets K=n so is basically leave-one-out



######################################## POINT OF NO RETURN RE: RECOVERY? PSME
## First, does cumulative likelihood plateau at some point? Pt of no return...?
# Plot partial dependence plots to see relationship btwn YEAR.DIFF & regen.
# Use random forest instead. First keep regen as numeric. 
data.psme$regen_psme <- as.numeric(as.character(data.psme$regen_psme)) 
# Create random forest, which warns of only two unique vals (0 & 1) in response.
# N.b., rug gives deciles of distribution of obs.
rf <- randomForest(regen_psme ~ YEAR.DIFF,
                   data = data.psme, importance = TRUE)
pdp::partial(rf, pred.var = "YEAR.DIFF",
             plot = TRUE, rug = TRUE)
# No super compelling pattern -- maybe post-15?
# I think yhat is relative logit contribution.

# Set to factor; prob = TRUE returns probability not logit.
data.psme$regen_psme <- factor(data.psme$regen_psme, ordered =TRUE)
rf <- randomForest(regen_psme ~ YEAR.DIFF,
                   data = data.psme, importance = TRUE)
pdp::partial(rf, pred.var = "YEAR.DIFF",
             plot = TRUE, rug = TRUE,
             prob = TRUE)
# Something clearly happens after yr 10. Probability of getting level 1 (0) declines?


######################################## JUST LOOK AT GTE 10 YRS POST-FIRE. PSME
data.psme$regen_psme <- as.numeric(as.character(data.psme$regen_psme)) 
## Keep sites visited 10 or more years post-fire & gurantee some seed source 
moo <- data.psme[data.psme$YEAR.DIFF > 9 & data.psme$BALive_psme > 0,]
# data.psme$regen_psme <- factor(data.psme$regen_psme, ordered = FALSE)
moo$BAProp_psme <- moo$BALive_psme/moo$BALiveTot


## Is there a pattern to failure?
# p <- ggplot() +
#   geom_sf(data = Wsts) +
#   geom_point(data = moo,
#              aes(x = LON_FS, y = LAT_FS, col = regen_psme)) +
#   scale_color_manual(values = c("blue", "yellow"),
#                      labels = c("no regen", "regen"))
# p # Maybe somethign in sky islands? Particular fire...?


## See what strongest predictors are of this dataset.
# First, scale variables for comparison. <-- HOLD OFF TIL MODELS SETTLED
# var.scale <- moo %>%
#   dplyr::select(BALive_psme, MAP_1995, CMD_CHNG, FIRE.SEV) %>%
#   scale() %>% as.data.frame()
# voo <- data.frame(regen_psme = moo$regen_psme, var.scale)
# remove(var.scale)

## What predicts regen failure in the long-run?
mod.psme = glm(regen_psme ~ BALive_psme + MAP_1995 + CMD_1995 + def59_z_3 + FIRE.SEV,
               data = moo, family = binomial(link = "logit"))
summary(mod.psme)  


## Partial dependence plots
# Can't plot partial dependence with binomial (just straight line); see rf
moo$regen_psme <- factor(moo$regen_psme, ordered = FALSE) ; levels(moo$regen_psme)[1]
rf <- randomForest(regen_psme  ~ BALive_psme + MAP_1995 + CMD_1995 + def59_z_3 + FIRE.SEV,
                   data = moo, importance = TRUE)
pdp::partial(rf, pred.var = "CMD_1995",
             plot = TRUE, rug = TRUE, prob = TRUE)
# ^ As deficit increases past 400, you're less likely to see out outcome of 1 -- i.e., 1 regen (numeric)

moo$regen_psme <- as.numeric(as.character(moo$regen_psme))
rf <- randomForest(regen_psme  ~ BALive_psme + MAP_1995 + CMD_1995 + def59_z_3 + FIRE.SEV,
                   data = moo, importance = TRUE)
pdp::partial(rf, pred.var = "CMD_1995",
             plot = TRUE, rug = TRUE, prob = TRUE)
# ^ As deficit increases past 400, you're less likely to see out outcome of 1 -- i.e., 1 regen (numeric)


## Eval model
# Standard plots
plot(mod.psme)

## Deviance over null
mod.psme.null <- update(mod.psme, regen_psme ~ 1)
(deviance(mod.psme.null) - deviance(mod.psme))/deviance(mod.psme.null) 

## HL GOF(best bet when you don't necessarily have replicated precitor values)
hl <- hoslem.test(moo$regen_psme, fitted(mod.psme), g=6) # g=num covariates + 1
cbind(hl$observed, hl$expected)
hl # 0.6527 high p-value says no evidence of lack-of-fit.

# ROC
regen_pred=predict(mod.psme, type ="response")
moo$regen_pred <- regen_pred
roccurve <- roc(regen_psme ~ regen_pred, data = moo)
par(mfrow=c(1,1))
plot(roccurve)
auc(roccurve)


# Prediction rate on orig data (confusion matrix) 
regen_pred <- predict(mod.psme, type = "response")
thresh <- 0.5 # define threshold for categorizing predicted probabilities.
# Then cut predicted probabilities into categories 
predCat <- cut(regen_pred, breaks=c(-Inf, thresh, Inf), labels=c("0", "1"))
# Create confusion matrix
confusn <- table(predCat, moo$regen_psme, dnn=c("predicted", "actual"))
addmargins(confusn)


# Alt: caret package
regen_pred <- predict(mod.psme, type = "response")
confusionMatrix(data = as.factor(as.numeric(regen_pred>0.5)), # needs factor but set num for 0/1 first
                reference = as.factor(moo$regen_psme), # needs factor
                dnn = c("predicted", "actual"))

# CV
cv.glm(moo, mod.psme, K=10)$delta # 10-fold cross-validation: raw & adjusted; nb no K sets K=n so is basically leave-one-out




######################################################## MAP IT
# Map should have all plots then gradually fade as regen occurs, with time since fire (not actual year).
# What remains are all plots that have no regen.
# Alt: Alternate between plots that have and don't have regen after 10 years.
# Trying to animate... to no avail.

## Try to animate based on decline in regen likelihood
install.packages("gganimate")
install.packages("gifski")
library(gganimate)
library(gifski)




p <- ggplot() +
  # state outlines
  geom_sf(data = Wsts) +
  geom_point(data = as.data.frame(coords), aes(x = lon, y = lat)) +
  theme_map()
p

moo$time <- as.numeric(as.character(moo$regen_pipo))+1
p <- ggplot() +
  # state outlines
  geom_sf(data = Wsts) +
  geom_point(data = as.data.frame(moo), aes(x = LON_FS, y = LAT_FS,
                                            # group = regen_pipo,
                                            color = regen_pipo),
                                            size = 5, alpha = 0.7) +
  theme_map()
p


## Trying to slow frames down
anim <- p + transition_states(regen_pipo, # will be based on group, here regen_pipo
                              transition_length = 0, # nix this so labels line up with state
                              state_length = 1) +
  # shadow_mark(alpha = 0.3) + # would leave shadow, but 2-state wrap makes weird 
  # enter_fade() + 
  # exit_shrink() + # would make points shrink
  ggtitle('Now showing {closest_state}')


animate(anim, fps=50, renderer= gifski_renderer(paste0(out.dir,"animation_",currentDate,".gif"))) # Transition/state lenghts are relative. Here, specify frames/sec... 




## Saving stuff
install.packages("magick")
library(magick)
#> Linking to ImageMagick 6.9.9.39
#> Enabled features: cairo, fontconfig, freetype, lcms, pango, rsvg, webp
#> Disabled features: fftw, ghostscript, x11
image <- animate(p)
image_write(image, paste0(out.dir,"animation_",currentDate,".gif"))
# ^ doesn't work
animate(p, nframes = 24, renderer = gifski_renderer(paste0(out.dir,"animation_",currentDate,".gif")))



## Possible mtbs map
fire <- st_read("//goshawk.sefs.uw.edu/Space_Lawler/Shared/BackedUp/Caitlin/NW CASC/Dir/WA_Large_Fires/wa_lrg_fires.shp")
fire <- fire[,c("FIRENAME", "YEAR")]
class(fire) # sfdf
plot(st_geometry(fire))
plot(st_geometry(fire[fire$FIRENAME == "Tripod (Tripod Complex)" ,]))

min(fire$YEAR)
max(fire$YEAR)

yr.vec <- c(1973, 2005, 2010, 2015, 2020)
fire$FIRE.BIN <- findInterval(fire$YEAR, vec=yr.vec, rightmost.closed=TRUE)


p <- ggplot() +
  # state outlines
  geom_sf(data = Wsts[Wsts$NAME == "Washington",]) +
  geom_sf(data = fire, aes(fill = FIRE.BIN)) +
  scale_fill_gradient(low = "#56B1F7", high = "#f79c56")
p

?scale_color_brewer 

scale_color_manual(values = c("yellow", "orange", "light green", "dark green"),
                   labels = c("3: <BA <CMD", "4: <BA >CMD", "6: >BA <CMD", "7: >BA >CMD"))
p