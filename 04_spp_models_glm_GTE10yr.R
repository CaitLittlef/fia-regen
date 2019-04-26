## What causes some places to fail in the long run versus recover?

currentDate <- Sys.Date()

## Load data; remove extraneous column if necessary
data.pipo <- read.csv("data.pipo_2019-04-24.csv") ; data.pipo$X <- NULL
# identical(data.pipo$FIRE.YR.x, data.pipo$FIRE.YR.y)
data.pipo$FIRE.YR <- data.pipo$FIRE.YR.x
data.pipo$FIRE.YR.x <- NULL ; data.pipo$FIRE.YR.y <- NULL

data.psme <- read.csv("data.psme_2019-04-24.csv") ; data.psme$X <- NULL
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
# Something happens around 15 years?



######################################## JUST LOOK AT GTE 10 YRS POST-FIRE. PIPO
## Keep sites visited 10 or more years post-fire & gurantee some seed source 
# After model selected, scale variables to be able to compare
data.pipo %>% count(YEAR.DIFF > 9)
# data.pipo %>% count(YEAR.DIFF > 14)
data.pipo$regen_pipo <- as.numeric(as.character(data.pipo$regen_pipo))
boo <- data.pipo[data.pipo$YEAR.DIFF > 9,]# & data.pipo$BALive_pipo > 0,]
boo$BAProp_pipo <- boo$BALive_pipo/boo$BALiveTot



###### Model selection: proceed with retaining/dropping out variables
### Orig
boo$FIRE.SEV <- factor(boo$FIRE.SEV, ordered = TRUE)
mod.pipo = glm(regen_pipo ~
                 BALive_pipo + BALiveTot
               + def.tc
               + tmax.tc
               + ppt.tc
               + CMD_CHNG 
               + def59_z_1
               + def59_z_12
               + def59_z_13
               + def59_z_14
               + def59_z_15
               # + ELEV
               + REBURN 
               + FIRE.SEV
               ,
               data = boo, family = binomial)
summary(mod.pipo)
# stepAIC(mod.pipo)
plotmo(mod.pipo, pmethod = "partdep")
## !! WHY DO PDPs CHANGE WITH EACH NEW POST-FIRE Z-SCORE YR?? !! ##

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
(AUC.5fld.v1 <- rowMeans(AUC)) 


### Least signif: all terraclimate variables; leave in reburn to see
# Update model
mod.pipo.v2 <- update(mod.pipo, regen_pipo ~ BALive_pipo + def59_z_15 + FIRE.SEV + REBURN)
summary(mod.pipo.v2)
AIC(mod.pipo.v2)
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
(AUC.5fld.v2 <- rowMeans(AUC)) 
rm(roccurve, area, AUC)

### Drop reburn, too 
mod.pipo.v3 <- update(mod.pipo, regen_pipo ~ BALive_pipo + def59_z_15 + FIRE.SEV)
summary(mod.pipo.v3)
AIC(mod.pipo.v3)
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
(AUC.5fld.v3 <- rowMeans(AUC)) # 0.8079471
rm(roccurve, area, AUC)


### Keep only z-score deficit 
mod.pipo.v4 <- update(mod.pipo, regen_pipo ~ def59_z_15 )
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


### Compare options
AUC.5fld.v1 # highest AUC
AUC.5fld.v2 # high AUC
AUC.5fld.v3 # med AUC
AUC.5fld.v4 # lowest AUC

cv.glm(boo, mod.pipo, K=5)$delta  # Not specifying # folds runs leave-one-out
cv.glm(boo, mod.pipo.v2, K=5)$delta # 
cv.glm(boo, mod.pipo.v3, K=5)$delta # 
cv.glm(boo, mod.pipo.v4, K=5)$delta # 

AIC(mod.pipo, mod.pipo.v2, mod.pipo.v3, mod.pipo.v4)
# v3 < v4 < v2 < v1 

plotmo(mod.pipo.v3)

# details re: customzing pdps here https://bgreenwell.github.io/pdp/articles/pdp.html
# partial default is NO plot (plot=TRUE plots it). Here, add and manipulate w/ autoplot
p1 <- mod.pipo.v3 %>%  
  partial(pred.var = "BALive_pipo", prob = TRUE) %>% 
    autoplot(smooth = TRUE, 
           col = "#046C9A",
           xlab = "Live PIPO BA",
           ylab = "Probability of juvenile presence") +
  coord_cartesian(ylim=c(0,1)) +
  theme_bw(base_size = 18)
p1

p2 <- mod.pipo.v3 %>%  
  partial(pred.var = "def59_z_15", prob = TRUE) %>%
  autoplot(smooth = TRUE,
           xlab = "Post-fire deficit z-score",
           ylab = "Probability of juvenile presence") +
  coord_cartesian(ylim=c(0,1)) +
  theme_bw(base_size = 18)
p2

p3 <- mod.pipo.v3 %>%  
  partial(pred.var = "FIRE.SEV", prob = TRUE) %>%
  autoplot(smooth = TRUE,
           col = "#046C9A",
           cex = 5,
           xlab = "Fire severity",
           ylab = "Probability of juvenile presence") +
  coord_cartesian(ylim=c(0,1)) +
  theme_bw(base_size = 18)
p3

dev.off()
# tiff(paste0(out.dir,"pipo_GTE10_pdps_",currentDate,".tiff"),
#       width = 1000, height = 400, units = "px")
# grid.arrange(p1, p2, p3, ncol = 3)
# dev.off()
# tiff(paste0(out.dir,"pipo_GTE10_pdps_p1p2_",currentDate,".tiff"),
#       width = 660, height = 400, units = "px")
# grid.arrange(p1, p2, ncol = 2)
# dev.off()
# tiff(paste0(out.dir,"pipo_GTE10_pdps_p3_",currentDate,".tiff"),
#       width = 330, height = 400, units = "px")
# grid.arrange(p3, ncol = 1)
# dev.off()

##############################################
# IN HERE< FIGURE OUT WAY TO DISAPLY PARTIALS WITHOUT HAVING FIRE SEV & REBURN PLOT AREA EXTENDED
# install.packages("egg")
# library(egg)
# p_fixed <- set_panel_size(p3,
#                           width  = unit(10, "cm"),
#                           height = unit(4, "in"))
# grid.newpage()
# grid.draw(p_fixed)
# 
# 
# gl = lapply(list(p1,p2,p3,p4), ggplotGrob)     
# library(gtable)
# g = do.call(rbind, c(gl, size="first"))
# g$widths = do.call(unit.pmax, lapply(gl, "[[", "widths"))
# 
# grid.newpage()
# grid.draw(g) 
# 
# 
# lh <- rbind(abla_plot, laoc_plot, pico_plot, size = "first")
# rh <- rbind(pien_plot, pipo_plot, psme_plot, size = "first")
# g <- cbind(lh, rh, size = "first")
# 
# # install.packages("cowplot")
# library(cowplot)
# currentDate <- Sys.Date()
# plot_grid(g, legend, ncol = 1, align = "v", rel_heights = c(3, 0.25))
# dev.off()
##############################################################################################




boo$regen_pipo <- factor(boo$regen_pipo)
boo %>%
  count(regen_pipo)
## Is there a pattern to failure?

p <- ggplot() +
  # state outlines
  geom_sf(data = NAmer, color = "#808B96", fill = "white") +
  geom_sf(data = IntWsts, color = "#808B96", fill = "#EAECEE") +
  coord_sf(xlim = c(-121, -100), ylim = c(30, 50), expand = FALSE) +
  geom_point(data = boo, aes(x = LON_FS, y = LAT_FS, col = regen_pipo),
             size = 4, alpha = 0.7) +
  scale_color_manual(values = c("#F98400", "#046C9A"),
                     labels = c("No juveniles present", "Juveniles present")) + 
  theme_bw(base_size = 18) +
  theme(panel.grid.major = element_line(color = "#808B96"), # blend lat/long into background
        panel.background = element_rect(fill = "#808B96"),
        axis.title = element_blank(),
        legend.title = element_blank(),
        legend.background = element_rect(color = "#808B96"),
        legend.justification=c(0,0), # defines which side oflegend .position coords refer to 
        legend.position=c(0,0),
        legend.text = element_text(size=12))
p

tiff(paste0(out.dir,"pipo_GTE10yr_juvNojuv_map_",currentDate,".tiff"),
   width = 450, height = 600, units = "px")
p
dev.off()




##############################################################################
######################################## POINT OF NO RETURN RE: RECOVERY? PSME

# Orig glm tree with partitions
plotmo(glm.tree.psme, type = "response") # gives probabilities, not log-odds

# Simple glm tree without partitions
tree <- glmtree(regen_psme ~ YEAR.DIFF, data = data.psme, family = "binomial", minsplit = 50, ordinal = "L2")
plotmo(tree, type = "response") # gives probabilities; plotmo sets other vars (if there) to median.
plotmo(tree, type = "response", pmethod = "partdep") # gives probabilities; pdps use avg.
plotmo(tree, type = "link", pmethod = "partdep") # gives log-odds; pdps use avg.


######################################## JUST LOOK AT GTE 10 YRS POST-FIRE. PSME
## Keep sites visited 10 or more years post-fire & gurantee some seed source 
# After model selected, scale variables to be able to compare
data.psme %>% count(YEAR.DIFF > 9)
# data.psme %>% count(YEAR.DIFF > 14)
data.psme$regen_psme <- as.numeric(as.character(data.psme$regen_psme))
moo <- data.psme[data.psme$YEAR.DIFF > 9,] #& data.psme$BALive_psme > 0,]
moo$BAProp_psme <- moo$BALive_psme/moo$BALiveTot


## What predicts regen failure in the long-run?
mod.psme = glm(regen_psme ~ BALive_psme + ppt.tc + tmax.tc + def.tc + aet.tc + def59_z_13 + CMD_CHNG + ELEV + FIRE.SEV + REBURN, data = moo, family = binomial)
summary(mod.psme)  
plotmo(mod.psme)
stepAIC(mod.psme)

###### Model selection: proceed with retaining/dropping out variables
### Orig
moo$FIRE.SEV <- factor(moo$FIRE.SEV, ordered = TRUE)
mod.psme = glm(regen_psme ~
                 BALive_psme + BALiveTot
               + def.tc
               + tmax.tc
               + ppt.tc
               + CMD_CHNG 
               + def59_z_1
               + def59_z_12
               + def59_z_13
               + def59_z_14
               + def59_z_15
               # + ELEV
               + REBURN 
               + FIRE.SEV
               ,
               data = moo, family = binomial)
summary(mod.psme)
stepAIC(mod.psme)
plotmo(mod.psme)

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
(AUC.5fld.v1 <- rowMeans(AUC)) 
rm(roccurve, area, AUC)


stepAIC(mod.psme)
## KEep what stepAIC says
mod.psme.v2 <- update(mod.psme, regen_psme ~ BALive_psme + def.tc)
summary(mod.psme.v2)
AIC(mod.psme.v2) 

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
(AUC.5fld.v2 <- rowMeans(AUC)) 


## In original summary, only def.tc was signif
mod.psme.v3 <- update(mod.psme, regen_psme ~ def.tc)
summary(mod.psme.v3)
AIC(mod.psme.v3) 

## CV to get prediction error rate
cv.glm(moo, mod.psme.v3, K=5)$delta # 5-fold cross-validation: raw & adjusted
# nb no K sets K=n so is basically leave-one-out

## 5-fold AUC
moo<-moo[sample(nrow(moo)),]
folds <- cut(seq(1,nrow(moo)),breaks=5,labels=FALSE)
AUC <- NULL
for(i in 1:5){
  foldIndexes <- which(folds==i,arr.ind=TRUE)
  foldData <- moo[foldIndexes, ]
  mod <- update(mod.psme.v3, data = foldData)
  foldPred <- predict(mod, foldData, type = "response")
  roccurve <- roc(regen_psme ~ foldPred, data = foldData) 
  area <- auc(roccurve)
  AUC <- cbind(AUC, area)
}
(AUC.5fld.v3 <- rowMeans(AUC)) 



### Candidate comparison
AUC.5fld.v1 
AUC.5fld.v2
AUC.5fld.v3

AIC(mod.psme, mod.psme.v2, mod.psme.v3)

cv.glm(moo, mod.psme, K=5)$delta 
cv.glm(moo, mod.psme.v2, K=5)$delta 
cv.glm(moo, mod.psme.v3, K=5)$delta 


### PLOT PARTIALS
p1 <- mod.psme.v2 %>%  
  partial(pred.var = "BALive_psme", prob = TRUE) %>%
  autoplot(smooth = TRUE,
           xlab = "Live PSME BA",
           ylab = "Probability of juvenile presence") +
  coord_cartesian(ylim=c(0,1)) +
  theme_bw(base_size = 18)
p1

p2 <- mod.psme.v2 %>%  
  partial(pred.var = "def.tc", prob = TRUE) %>%
  autoplot(smooth = TRUE,
           xlab = "Deficit",
           ylab = "Probability of juvenile presence") +
  coord_cartesian(ylim=c(0,1)) +
  theme_bw(base_size = 18)
p2

# dev.off()
# tiff(paste0(out.dir,"psme_GTE10_pdps_",currentDate,".tiff"),
#       width = 660, height = 400, units = "px")
# grid.arrange(p1, p2, ncol = 2)
# dev.off()
moo %>% count(regen_psme)





### Is there a pattern to failure?
moo$regen_psme <- factor(moo$regen_psme)
moo %>% count(regen_psme)
moo %>% count()
## Is there a pattern to failure?

p <- ggplot() +
  # state outlines
  geom_sf(data = NAmer, color = "#808B96", fill = "white") +
  geom_sf(data = IntWsts, color = "#808B96", fill = "#EAECEE") +
  coord_sf(xlim = c(-121, -100), ylim = c(30, 50), expand = FALSE) +
  geom_point(data = moo, aes(x = LON_FS, y = LAT_FS, col = regen_psme),
             size = 4, alpha = 0.7) +
  scale_color_manual(values = c("#F98400", "#046C9A"),
                     labels = c("No juveniles present", "Juveniles present")) + 
  theme_bw(base_size = 18) +
  theme(panel.grid.major = element_line(color = "#808B96"), # blend lat/long into background
        panel.background = element_rect(fill = "#808B96"),
        axis.title = element_blank(),
        legend.title = element_blank(),
        legend.background = element_rect(color = "#808B96"),
        legend.justification=c(0,0), # defines which side oflegend .position coords refer to 
        legend.position=c(0,0),
        legend.text = element_text(size=12))
p

# tiff(paste0(out.dir,"psme_GTE10yr_juvNojuv_map_",currentDate,".tiff"),
#    width = 450, height = 600, units = "px")
# p
# dev.off()


