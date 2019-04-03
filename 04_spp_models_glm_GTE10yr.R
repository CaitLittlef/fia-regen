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

# Look at smooth
gam.pipo <- gam(regen_pipo ~ s(YEAR.DIFF, k = 5), data = data.pipo, family = "binomial")
gam.psme <- gam(regen_psme ~ s(YEAR.DIFF, k = 5), data = data.psme, family = "binomial")
plotmo(gam.pipo)
plotmo(gam.psme)
# Something happens around 8 years?



######################################## JUST LOOK AT GTE 10 YRS POST-FIRE. PIPO
## Keep sites visited 8 or more years post-fire & gurantee some seed source 
# After model selected, scale variables to be able to compare
data.pipo$regen_pipo <- as.numeric(as.character(data.pipo$regen_pipo))
boo <- data.pipo[data.pipo$YEAR.DIFF > 9 & data.pipo$BALive_pipo > 0,]
boo$BAProp_pipo <- boo$BALive_pipo/boo$BALiveTot



###### Model selection: proceed with retaining/dropping out variables
### Orig
mod.pipo = glm(regen_pipo ~ BALive_pipo + ppt.tc + tmax.tc + def.tc + aet.tc + def59_z_13 + FIRE.SEV + REBURN, data = boo, family = binomial)
summary(mod.pipo)
AIC(mod.pipo) # 228.9698


## CV to get prediction error rate
cv.glm(boo, mod.pipo, K=5)$delta # 5-fold cross-validation: raw & adjusted
# nb no K sets K=n so is basically leave-one-out

## 5-fold AUC. nb may fail if not all factor levels represented -- run again.
boo<-boo[sample(nrow(boo)),]
folds <- cut(seq(1,nrow(boo)),breaks=5,labels=FALSE)
AUC <- NULL
for(i in 1:5){
  foldIndexes <- which(folds==i,arr.ind=TRUE)
  foldData <- boo[foldIndexes, ]
  mod <- update(mod.pipo, data = foldData)
  foldPred <- predict(mod, foldData, type = "response")
  roccurve <- roc(regen_pipo ~ foldPred, data = foldData) 
  area <- auc(roccurve)
  AUC <- cbind(AUC, area)
}
(AUC.5fld.v1 <- rowMeans(AUC)) # 0.7999728 
rm(roccurve, area, AUC)


### Least signif: all terraclimate variables
# Update model
mod.pipo.v2 <- update(mod.pipo, regen_pipo ~ BALive_pipo + def59_z_13 + FIRE.SEV + REBURN)
summary(mod.pipo.v2)
AIC(mod.pipo.v2) # 221.2348
plotmo(mod.pipo.v2)

## CV to get prediction error rate
cv.glm(boo, mod.pipo.v2, K=5)$delta # 5-fold cross-validation: raw & adjusted
# nb no K sets K=n so is basically leave-one-out

## 5-fold AUC
boo<-boo[sample(nrow(boo)),]
folds <- cut(seq(1,nrow(boo)),breaks=5,labels=FALSE)
AUC <- NULL
for(i in 1:5){
  foldIndexes <- which(folds==i,arr.ind=TRUE)
  foldData <- boo[foldIndexes, ]
  mod <- update(mod.pipo.v2, data = foldData)
  foldPred <- predict(mod, foldData, type = "response")
  roccurve <- roc(regen_pipo ~ foldPred, data = foldData) 
  area <- auc(roccurve)
  AUC <- cbind(AUC, area)
}
(AUC.5fld.v2 <- rowMeans(AUC)) # 0.7269562
rm(roccurve, area, AUC)

### Drop reburn, too 
mod.pipo.v3 <- update(mod.pipo, regen_pipo ~ BALive_pipo + def59_z_13 + FIRE.SEV)
summary(mod.pipo.v3)
AIC(mod.pipo.v3) # 220.963
plotmo(mod.pipo.v3)

## CV to get prediction error rate
cv.glm(boo, mod.pipo.v3, K=5)$delta # 5-fold cross-validation: raw & adjusted
# nb no K sets K=n so is basically leave-one-out

## 5-fold AUC
boo<-boo[sample(nrow(boo)),]
folds <- cut(seq(1,nrow(boo)),breaks=5,labels=FALSE)
AUC <- NULL
for(i in 1:5){
  foldIndexes <- which(folds==i,arr.ind=TRUE)
  foldData <- boo[foldIndexes, ]
  mod <- update(mod.pipo.v3, data = foldData)
  foldPred <- predict(mod, foldData, type = "response")
  roccurve <- roc(regen_pipo ~ foldPred, data = foldData) 
  area <- auc(roccurve)
  AUC <- cbind(AUC, area)
}
(AUC.5fld.v3 <- rowMeans(AUC)) # 0.722767
rm(roccurve, area, AUC)


### Keep only z-score deficit 
mod.pipo.v4 <- update(mod.pipo, regen_pipo ~ def59_z_13 )
summary(mod.pipo.v4)
AIC(mod.pipo.v4) # 220.963
plotmo(mod.pipo.v4)

## CV to get prediction error rate
cv.glm(boo, mod.pipo.v4, K=5)$delta # 5-fold cross-validation: raw & adjusted
# nb no K sets K=n so is basically leave-one-out

## 5-fold AUC
boo<-boo[sample(nrow(boo)),]
folds <- cut(seq(1,nrow(boo)),breaks=5,labels=FALSE)
AUC <- NULL
for(i in 1:5){
  foldIndexes <- which(folds==i,arr.ind=TRUE)
  foldData <- boo[foldIndexes, ]
  mod <- update(mod.pipo.v4, data = foldData)
  foldPred <- predict(mod, foldData, type = "response")
  roccurve <- roc(regen_pipo ~ foldPred, data = foldData) 
  area <- auc(roccurve)
  AUC <- cbind(AUC, area)
}
(AUC.5fld.v4 <- rowMeans(AUC)) # 0.6179613
rm(roccurve, area, AUC)

AUC.5fld.v1 # highest AUC
AUC.5fld.v2 # high AUC
AUC.5fld.v3 # med AUC
AUC.5fld.v4 # lowest AUC

cv.glm(boo, mod.pipo, K=5)$delta # lowest error rate
cv.glm(boo, mod.pipo.v2, K=5)$delta # lowest error rate
cv.glm(boo, mod.pipo.v3, K=5)$delta # still pretty low error rate
cv.glm(boo, mod.pipo.v4, K=5)$delta # still pretty low error rate

AIC(mod.pipo, mod.pipo.v2, mod.pipo.v3, mod.pipo.v4)
# v3 < v2 < v4 < v1 w/ v2 and v3 quite close.

cv.glm(boo, mod.pipo, K=5)$delta # highest error rate
# Not specifying # folds runs leave-one-out
cv.glm(boo, mod.pipo)$delta # 
cv.glm(boo, mod.pipo.v2)$delta # 
cv.glm(boo, mod.pipo.v3)$delta # lowest
cv.glm(boo, mod.pipo.v4)$delta # 

# toss-up btwn 2 & 3, but all told 3 is best
# 3 is more parsimonious w/o REBURN so retain tbat model.
# But relatinoship w/ reburn interesting.

mod.pipo.final <- mod.pipo.v3
plotmo(mod.pipo.final)

# What about only BA > 35 which was breakpt in trees
poo <- boo %>% filter(BALive_pipo > 35)
mod.temp <- update(mod.pipo.final, data = poo)
summary(mod.temp)
plotmo(mod.temp)

# What about N of 40? which was breakpt in trees
poo <- boo %>% filter(LAT_FS > 40)
mod.elev = glm(regen_pipo ~ BALive_pipo + def59_z_13 + FIRE.SEV + ELEV,
               data = poo, family = binomial)
summary(mod.elev)
plotmo(mod.temp)


## What about interactions? # Maybe FIRE.SEV & Z-score make sense, but nix.
# mod.pipo.ax = glm(regen_pipo ~ BALive_pipo * def59_z_13 * FIRE.SEV * REBURN, data = boo, family = binomial)
# summary(mod.pipo.ax)



boo$regen_pipo <- factor(boo$regen_pipo)
## Is there a pattern to failure?
p <- ggplot() +
  geom_sf(data = Wsts) +
  geom_point(data = boo,
             aes(x = LON_FS, y = LAT_FS, col = regen_pipo)) +
  scale_color_manual(values = c("blue", "yellow"),
                     labels = c("no regen", "regen"))
p # Maybe somethign in sky islands? Particular fire...?





######################################## POINT OF NO RETURN RE: RECOVERY? PSME

# Orig glm tree with partitions
plotmo(glm.tree.psme, type = "response") # gives probabilities, not log-odds

# Simple glm tree without partitions
tree <- glmtree(regen_psme ~ YEAR.DIFF, data = data.psme, family = "binomial", minsplit = 50, ordinal = "L2")
plotmo(tree, type = "response") # gives probabilities; plotmo sets other vars (if there) to median.
plotmo(tree, type = "response", pmethod = "partdep") # gives probabilities; pdps use avg.
plotmo(tree, type = "link", pmethod = "partdep") # gives log-odds; pdps use avg.


######################################## JUST LOOK AT GTE 8 YRS POST-FIRE. PSME
## Keep sites visited 8 or more years post-fire & gurantee some seed source 
# After model selected, scale variables to be able to compare
data.psme$regen_psme <- as.numeric(as.character(data.psme$regen_psme))
moo <- data.psme[data.psme$YEAR.DIFF > 7 & data.psme$BALive_psme > 0,]
moo$BAProp_psme <- moo$BALive_psme/moo$BALiveTot


## What predicts regen failure in the long-run?
mod.psme = glm(regen_psme ~ BALive_psme + ppt.tc + tmax.tc + def.tc + aet.tc + def59_z_13 + FIRE.SEV + REBURN, data = moo, family = binomial)
summary(mod.psme)  
plotmo(mod.psme)


###### Model selection: proceed with retaining/dropping out variables
### Orig
mod.psme = glm(regen_psme ~ BALive_psme + ppt.tc + tmax.tc + def.tc + aet.tc + def59_z_13 + FIRE.SEV + REBURN, data = moo, family = binomial)
summary(mod.psme)
AIC(mod.psme) # 260.8357

## CV to get prediction error rate
cv.glm(moo, mod.psme, K=5)$delta # 5-fold cross-validation: raw & adjusted
# nb no K sets K=n so is basically leave-one-out

## 5-fold AUC
moo<-moo[sample(nrow(moo)),]
folds <- cut(seq(1,nrow(moo)),breaks=5,labels=FALSE)
AUC <- NULL
for(i in 1:5){
  foldIndexes <- which(folds==i,arr.ind=TRUE)
  foldData <- moo[foldIndexes, ]
  mod <- update(mod.psme, data = foldData)
  foldPred <- predict(mod, foldData, type = "response")
  roccurve <- roc(regen_psme ~ foldPred, data = foldData) 
  area <- auc(roccurve)
  AUC <- cbind(AUC, area)
}
(AUC.5fld.v1 <- rowMeans(AUC)) # 0.8445903
rm(roccurve, area, AUC)


stepAIC(mod.psme)
# Suggests keeping in on;ly ppt.tc and def.tc
mod.psme.v2 <- update(mod.psme, regen_psme ~ ppt.tc + def.tc)

AIC(mod.psme.v2) # 251.1467

## CV to get prediction error rate
cv.glm(moo, mod.psme.v2, K=5)$delta # 5-fold cross-validation: raw & adjusted
# nb no K sets K=n so is basically leave-one-out

## 5-fold AUC
moo<-moo[sample(nrow(moo)),]
folds <- cut(seq(1,nrow(moo)),breaks=5,labels=FALSE)
AUC <- NULL
for(i in 1:5){
  foldIndexes <- which(folds==i,arr.ind=TRUE)
  foldData <- moo[foldIndexes, ]
  mod <- update(mod.psme.v2, data = foldData)
  foldPred <- predict(mod, foldData, type = "response")
  roccurve <- roc(regen_psme ~ foldPred, data = foldData) 
  area <- auc(roccurve)
  AUC <- cbind(AUC, area)
}
(AUC.5fld.v2 <- rowMeans(AUC)) # 0.7213352
rm(roccurve, area, AUC)


summary(mod.psme)

AUC.5fld.v1 
AUC.5fld.v2 

AIC(mod.psme, mod.psme.v2)

cv.glm(moo, mod.psme, K=5)$delta 
cv.glm(moo, mod.psme.v2, K=5)$delta 

cv.glm(moo, mod.psme)$delta 
cv.glm(moo, mod.psme.v2)$delta 


moo$regen_psme <- factor(moo$regen_psme)
## Is there a pattern to failure?
p <- ggplot() +
  geom_sf(data = Wsts) +
  geom_point(data = moo,
             aes(x = LON_FS, y = LAT_FS, col = regen_psme)) +
  scale_color_manual(values = c("blue", "yellow"),
                     labels = c("no regen", "regen"))
p # Maybe somethign in sky islands? Particular fire...?








#### RANDOM FORESTS: DON'T USE BUT YOU DO SEE MUCH BIGGER THRESHOLD EFFECT WITH DEFICIT Z-SCORE
# ## What predicts regen failure in the long-run?
# mod.pipo = glm(regen_pipo ~ BALive_pipo + ppt.tc + tmax.tc + def.tc + aet.tc + def59_z_13 + FIRE.SEV + REBURN, data = boo, family = binomial)
# summary(mod.pipo)  
# plotmo(mod.pipo)
# # Partial dependence plots -- note you see a much greater threshold with rf than glm.
# boo$regen_pipo <- factor(boo$regen_pipo, levels= c("1", "0"), ordered = FALSE) ; levels(boo$regen_pipo)[1]
# rf <- randomForest(regen_pipo ~ BALive_pipo + ppt.tc + tmax.tc + def.tc + aet.tc + def59_z_3 + FIRE.SEV,
#                    data = boo, importance = TRUE)
# pdp::partial(rf, pred.var = "def59_z_3",
#              plot = TRUE, rug = TRUE, prob = TRUE)
# # ^ As deficit increases, you're less likely to get level 1 of this factor -- i.e., regen.
# 
# boo$regen_pipo <- as.numeric(as.character(boo$regen_pipo))
# rf <- randomForest(regen_pipo  ~ BALive_pipo + MAP_1995 + CMD_1995 + def59_z_13 + FIRE.SEV,
#                    data = boo, importance = TRUE)
# pdp::partial(rf, pred.var = "def59_z_13",
#              plot = TRUE, rug = TRUE, prob = TRUE)
# # ^ As deficit increases, you're less likely to see out outcome of 1 -- i.e., 1 regen (numeric)
