## What causes some places to fail in the long run versus recover?
# Based on brt pdps, >14 yrs for pipo is when trajectory stabilizes
# N.b., see CV response re: pdps with logistic regression
# https://stats.stackexchange.com/questions/394762/partial-dependence-plot-for-glm-in-r-why-linear#394764

currentDate <- Sys.Date()

## Load data; remove extraneous column if necessary
data.pipo <- read.csv("data.pipo_2019-06-12.csv") ; data.pipo$X <- NULL
data.psme <- read.csv("data.psme_2019-06-12.csv") ; data.psme$X <- NULL

## Exclude sites w/o fire OR w/ fire.sev 5 & 6 (here NA)
data.pipo <- data.pipo[! is.na(data.pipo$FIRE.SEV) ,]
data.psme <- data.psme[! is.na(data.psme$FIRE.SEV) ,]

## Set variable classes
# data.pipo$regen_glm <- factor(data.pipo$regen_glm, ordered = FALSE)
data.pipo$regen_glm <- as.numeric(as.character(data.pipo$regen_glm)) 
# data.psme$regen_psme <- factor(data.psme$regen_psme, ordered = FALSE)
data.psme$regen_psme <- as.numeric(as.character(data.psme$regen_psme))

data.pipo$FIRE.SEV <- factor(data.pipo$FIRE.SEV, ordered = TRUE)
# data.pipo$FIRE.SEV <- as.numeric(data.pipo$FIRE.SEV) 
data.psme$FIRE.SEV <- factor(data.psme$FIRE.SEV, ordered = TRUE)
# data.psme$FIRE.SEV <- as.numeric(data.psme$FIRE.SEV, ordered = TRUE)

data.pipo$REBURN <- factor(data.pipo$REBURN, ordered = TRUE)
# data.pipo$REBURN <- as.numeric(data.pipo$REBURN) 
data.pipo$REBURN <- factor(data.pipo$REBURN, ordered = TRUE)
# data.psme$REBURN <- as.numeric(data.psme$REBURN) 

# Keep only candidate variables
data.pipo <- data.pipo %>%
  dplyr::select(regen_glm, BALive_glm_m_m, YEAR.DIFF, def.tc, tmax.tc, ppt.tc, CMD_CHNG,
                def59_z_max15, DUFF_DEPTH_cm, LITTER_DEPTH_cm, FIRE.SEV, REBURN)
head(data.pipo)                

data.psme <- data.psme %>%
  dplyr::select(regen_psme, BALive_psme_m, YEAR.DIFF, def.tc, tmax.tc, ppt.tc, CMD_CHNG,
                def59_z_max15, DUFF_DEPTH_cm, LITTER_DEPTH_cm, FIRE.SEV, REBURN)
head(data.psme)    


###############################
##GLM SETUP ###################
###############################

## PIPO OR PSME??
data.glm <- data.pipo %>%
  rename(regen_glm = regen_pipo,
         BALive_glm_m = BALive_pipo_m) %>%
  filter(YEAR.DIFF > 14) ; sp <- c("pipo")

# 
# data.glm <- data.psme %>%
#   rename(regen_glm = regen_psme,
#          BALive_glm_m = BALive_psme_m) %>%
#   filter(YEAR.DIFF > 14) ; sp <- c("psme")

# Which sp?
print(sp)
count(data.glm,regen_glm)


# Make sure all factor levels are represented
count(data.glm, FIRE.SEV)
count(data.glm, REBURN) # Only has single level (N) for pipo so exclude (else glm won't run)
explan.vars.glm <- c("BALive_glm_m",
                      "def.tc",
                      "tmax.tc",
                      "ppt.tc",
                      "CMD_CHNG",
                      "def59_z_max15",
                      "DUFF_DEPTH_cm",
                      "LITTER_DEPTH_cm",
                      "FIRE.SEV")



#########################################
##GLM MODEL SELECTION####################
#########################################

vars <- paste(as.vector(explan.vars.glm), collapse = " + ")
(formula <- formula(paste("regen_glm ~ ", vars)))

mod.glm = glm(formula, data = data.glm, family = binomial)
summary(mod.glm)
stepAIC(mod.glm)
# 5-fold cross-validation error rate: raw & adjusted; no k sets k=n (~leave-one-out)
cv.glm(data.glm, mod.glm, K=5)$delta 
# auc (not cross-validated -- see CV auc way below)
pred <- predict(mod.glm, data.glm, type = "response")
roccurve <- roc(data.glm$regen_glm ~ pred)
(auc.v1 <- pROC::auc(roccurve))

as.numeric(as.character(auc.v1)) # 0.7251852
auc.v1 <- as.numeric(as.character(auc.v1))

# Re-write variable vector based on which one ought to be dropped
explan.vars.glm <- c("BALive_glm_m",
                     "def.tc",
                     "tmax.tc",
                     "ppt.tc",
                     "CMD_CHNG",
                     "def59_z_max15",
                     "DUFF_DEPTH_cm",
                     "LITTER_DEPTH_cm",
                     "FIRE.SEV")

# EACH TIME RE-LOAD EXPLAN.VARS.GLM ABOVE!!
# explan.vars.glm <- explan.vars.glm[-1] # drop BA
# explan.vars.glm <- explan.vars.glm[-c(1,3)] # drop BA, tmax
# explan.vars.glm <- explan.vars.glm[-c(1,3,5)] # drop BA, tmax, CMD_CHNG
explan.vars.glm <- explan.vars.glm[-c(1,2,3,5)] # drop BA, tmax, CMD_CHNG, def
# ^ THIS IS FINAL

drops <- NULL
aucs <- NULL

for (v in 1:length(explan.vars.glm)){
  drop <- explan.vars.glm[v]
  vars <- paste(as.vector(explan.vars.glm[-c(v)]), collapse = " + ")
  (formula <- formula(paste("regen_glm ~ ", vars)))
  mod.glm = glm(formula, data = data.glm, family = binomial)
  drops <- c(drops, drop)
  pred <- predict(mod.glm, data.glm, type = "response")
  roccurve <- roc(data.glm$regen_glm ~ pred)
  auc <- pROC::auc(roccurve)
  aucs <- c(aucs, auc)
}  

# Evavluate which vars to drop on basis of AUC improvement and then re-run.
temp <- as.data.frame(cbind(drops, aucs))
temp$aucs <- as.numeric(as.character(temp$aucs))

# Drop?
temp[which.max(temp$aucs),] # 1 BALive_glm_m 0.7355556
temp[which.max(temp$aucs),]$aucs - 0.7251852 # improve prior auc by 0.01037036 
# Drop BA

# Drop?
temp[which.max(temp$aucs),] # 2 tmax.tc 0.7340741
temp[which.max(temp$aucs),]$aucs - 0.7355556 # drops prior auc by tiny -0.001481526 
# Drop tmax

# Drop?
temp[which.max(temp$aucs),] # 3 CMD_CHNG 0.7322222
temp[which.max(temp$aucs),]$aucs - 0.7340741 # drops prior auc by tiny -0.001851878 
# Drop CMD_CHNG

# Drop?
temp[which.max(temp$aucs),] # 1 def.tc 0.7322222
temp[which.max(temp$aucs),]$aucs - 0.7322222 # tiiiiiiny improvement in AUC 
# Drop def.

# Drop?
temp[which.max(temp$aucs),] # 5 FIRE.SEV 0.7007407
temp[which.max(temp$aucs),]$aucs - 0.7322222 # drops auc by -0.03148146 so DON'T drop FIRE.SEV.
# STOP HERE
 
explan.vars.glm
# [1] "ppt.tc"          "def59_z_max15"   "DUFF_DEPTH_cm"   "LITTER_DEPTH_cm" "FIRE.SEV"  

## Final glm
vars <- paste(as.vector(explan.vars.glm), collapse = " + ")
(formula <- formula(paste("regen_glm ~ ", vars)))

mod.glm.fin = glm(formula, data = data.glm, family = binomial)
summary(mod.glm.fin)
stepAIC(mod.glm.fin) # STEP AIC STILL SUGGESTS I COULD DROP MORE VARS...
# 5-fold cross-validation error rate: raw & adjusted; no k sets k=n (~leave-one-out)
cv.glm(data.glm, mod.glm.fin, K=5)$delta 
# auc (not cross-validated -- see CV auc way below)
pred <- predict(mod.glm.fin, data.glm, type = "response")
roccurve <- roc(data.glm$regen_glm ~ pred)
(auc.fin <- pROC::auc(roccurve))
as.numeric(as.character(auc.fin)) # 0.7322222
auc.fin <- as.numeric(as.character(auc.fin))

plotmo(mod.glm.fin, all2=TRUE) # Why opposite direction duff & litter?
tiff(paste0(out.dir, sp, "_yr10_pdps.tiff"))
plotmo(mod.glm.fin, all2=TRUE) # Why opposite direction duff & litter?
dev.off()

## But kinda weird to have litter & duff in opposite directions...
summary(mod.glm.fin)
stepAIC(mod.glm) # says retain only precip & duff/litter
stepAIC(mod.glm.fin) # says retain only precip & duff/litter

## Try out retaining only what stepAIC says
mod.glm.v2 <- update(mod.glm.fin, regen_glm ~ ppt.tc 
                     + DUFF_DEPTH_cm
                     + LITTER_DEPTH_cm) 

summary(mod.glm.v2)
# auc (not cross-validated)
pred <- predict(mod.glm.v2, data.glm, type = "response")
roccurve <- roc(data.glm$regen_glm ~ pred)
(auc.v2 <- pROC::auc(roccurve)) # 0.6659: worse than fin 0.7322
# Retain fin.

plotmo()

## MODEL V3
# Iteratively drop least signif variable 
mod.glm.v3 <- update(mod.glm, regen_glm  ~
                       BALive_glm_m
                     # + def.tc
                     + tmax.tc
                     + ppt.tc
                     # + CMD_CHNG
                     + def59_z_max15
                     + DUFF_DEPTH_cm
                     + LITTER_DEPTH_cm
                     + FIRE.SEV)
summary(mod.glm.v3)
AIC(mod.glm.v3)
cv.glm(data.glm, mod.glm.v3, K=5)$delta # 5-fold cross-validation: raw & adjusted

# auc (not cross-validated)
pred <- predict(mod.glm.v3, data.glm, type = "response")
roccurve <- roc(data.glm$regen_glm ~ pred)
(auc.v3 <- pROC::auc(roccurve))

## MODEL V4
# Iteratively drop least signif variable (X cmd_chng, x def.tc)
mod.glm.v4 <- update(mod.glm, regen_glm  ~
                       BALive_glm_m
                     # + def.tc
                     # + tmax.tc
                     + ppt.tc
                     # + CMD_CHNG
                     + def59_z_max15
                     + DUFF_DEPTH_cm
                     + LITTER_DEPTH_cm
                     + FIRE.SEV)
summary(mod.glm.v4)
AIC(mod.glm.v4)
cv.glm(data.glm, mod.glm.v4, K=5)$delta # 5-fold cross-validation: raw & adjusted

# auc (not cross-validated)
pred <- predict(mod.glm.v4, data.glm, type = "response")
roccurve <- roc(data.glm$regen_glm ~ pred)
(auc.v4 <- pROC::auc(roccurve))

## MODEL V5
# Iteratively drop least signif variable (X cmd_chng, x def.tc)
mod.glm.v5 <- update(mod.glm, regen_glm  ~
                       BALive_glm_m
                     # + def.tc
                     # + tmax.tc
                     # + ppt.tc
                     # + CMD_CHNG
                     + def59_z_max15
                     + DUFF_DEPTH_cm
                     + LITTER_DEPTH_cm
                     + FIRE.SEV)
summary(mod.glm.v5)
AIC(mod.glm.v5)
cv.glm(data.glm, mod.glm.v5, K=5)$delta # 5-fold cross-validation: raw & adjusted

# auc (not cross-validated)
pred <- predict(mod.glm.v5, data.glm, type = "response")
roccurve <- roc(data.glm$regen_glm ~ pred)
(auc.v5 <- pROC::auc(roccurve))
lrtest(mod.glm.v5, mod.glm.v4) # variance the same (p>0.05) so proceed with simpler


## MODEL V6
# Iteratively drop least signif variable
mod.glm.v6 <- update(mod.glm, regen_glm  ~
                       BALive_glm_m
                     # + def.tc
                     # + tmax.tc
                     # + ppt.tc
                     # + CMD_CHNG
                     + def59_z_max15
                     + DUFF_DEPTH_cm
                     + LITTER_DEPTH_cm)
                     # + FIRE.SEV)
summary(mod.glm.v6)
AIC(mod.glm.v6)
cv.glm(data.glm, mod.glm.v6, K=5)$delta # 5-fold cross-validation: raw & adjusted

# auc (not cross-validated)
pred <- predict(mod.glm.v6, data.glm, type = "response")
roccurve <- roc(data.glm$regen_glm ~ pred)
(auc.v6 <- pROC::auc(roccurve))
lrtest(mod.glm.v6, mod.glm.v5) # variance the same (p>0.05) so proceed with simpler


## MODEL V7
# Iteratively drop least signif variable
mod.glm.v7 <- update(mod.glm, regen_glm  ~
                       # BALive_glm_m
                     # + def.tc
                     # + tmax.tc
                     # + ppt.tc
                     # + CMD_CHNG
                     + def59_z_max15
                     + DUFF_DEPTH_cm
                     + LITTER_DEPTH_cm)
# + FIRE.SEV)
summary(mod.glm.v7)
AIC(mod.glm.v7)
cv.glm(data.glm, mod.glm.v7, K=5)$delta # 5-fold cross-validation: raw & adjusted

# auc (not cross-validated)
pred <- predict(mod.glm.v7, data.glm, type = "response")
roccurve <- roc(data.glm$regen_glm ~ pred)
(auc.v7 <- pROC::auc(roccurve))
lrtest(mod.glm.v7, mod.glm.v6) # variance the same (p>0.05) so proceed with simpler
# BUT THIS IS GETTING CRAZY!


## MODEL V7
# Iteratively drop least signif variable
mod.glm.v7 <- update(mod.glm, regen_glm  ~
                       # BALive_glm_m
                       # + def.tc
                       # + tmax.tc
                       # + ppt.tc
                       # + CMD_CHNG
                       + def59_z_max15
                     + DUFF_DEPTH_cm
                     + LITTER_DEPTH_cm)
# + FIRE.SEV)
summary(mod.glm.v7)
AIC(mod.glm.v7)
cv.glm(data.glm, mod.glm.v7, K=5)$delta # 5-fold cross-validation: raw & adjusted

# auc (not cross-validated)
pred <- predict(mod.glm.v7, data.glm, type = "response")
roccurve <- roc(data.glm$regen_glm ~ pred)
(auc.v7 <- pROC::auc(roccurve))
lrtest(mod.glm.v7, mod.glm.v6) # variance the same (p>0.05) so proceed with simpler
# BUT THIS IS GETTING CRAZY!




auc.v1
auc.v2
auc.v3
auc.v4
auc.v5
auc.v6
auc.v7







### Drop fire.sev, too 
mod.pipo.v3 <- update(mod.pipo, regen_glm ~ BALive_glm_m + def59_z_15 + FIRE.SEV)
summary(mod.pipo.v3)
AIC(mod.pipo.v3)
plotmo(mod.pipo.v3)

## CV to get prediction error rate
cv.glm(data.glm, mod.pipo.v3, K=5)$delta # 5-fold cross-validation: raw & adjusted
# nb no K sets K=n so is basically leave-one-out

## 5-fold AUC
data.glm<-data.glm[sample(nrow(data.glm)),]
folds <- cut(seq(1,nrow(data.glm)),breaks=5,labels=FALSE)
AUC <- NULL
for(i in 1:5){
  foldIndexes <- which(folds==i,arr.ind=TRUE)
  foldData <- data.glm[foldIndexes, ]
  mod <- update(mod.pipo.v3, data = foldData)
  foldPred <- predict(mod, foldData, type = "response")
  roccurve <- roc(regen_glm ~ foldPred, data = foldData) 
  area <- auc(roccurve)
  AUC <- cbind(AUC, area)
}
(AUC.5fld.v3 <- rowMeans(AUC)) # 0.8079471
rm(roccurve, area, AUC)


### Keep only z-score deficit 
mod.pipo.v4 <- update(mod.pipo, regen_glm ~ def59_z_15 )
summary(mod.pipo.v4)
AIC(mod.pipo.v4) # 220.963
plotmo(mod.pipo.v4)

## CV to get prediction error rate
cv.glm(data.glm, mod.pipo.v4, K=5)$delta # 5-fold cross-validation: raw & adjusted
# nb no K sets K=n so is basically leave-one-out

## 5-fold AUC
data.glm<-data.glm[sample(nrow(data.glm)),]
folds <- cut(seq(1,nrow(data.glm)),breaks=5,labels=FALSE)
AUC <- NULL
for(i in 1:5){
  foldIndexes <- which(folds==i,arr.ind=TRUE)
  foldData <- data.glm[foldIndexes, ]
  mod <- update(mod.pipo.v4, data = foldData)
  foldPred <- predict(mod, foldData, type = "response")
  roccurve <- roc(regen_glm ~ foldPred, data = foldData) 
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

cv.glm(data.glm, mod.pipo, K=5)$delta  # Not specifying # folds runs leave-one-out
cv.glm(data.glm, mod.pipo.v2, K=5)$delta # 
cv.glm(data.glm, mod.pipo.v3, K=5)$delta # 
cv.glm(data.glm, mod.pipo.v4, K=5)$delta # 

AIC(mod.pipo, mod.pipo.v2, mod.pipo.v3, mod.pipo.v4)
# v3 < v4 < v2 < v1 

plotmo(mod.pipo.v3)

# details re: customzing pdps here https://bgreenwell.github.io/pdp/articles/pdp.html
# partial default is NO plot (plot=TRUE plots it). Here, add and manipulate w/ autoplot
p1 <- mod.pipo.v3 %>%  
  partial(pred.var = "BALive_glm_m", prob = TRUE) %>% 
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




boo$regen_glm <- factor(boo$regen_glm)
boo %>%
  count(regen_glm)
## Is there a pattern to failure?

p <- ggplot() +
  # state outlines
  geom_sf(data = NAmer, color = "#808B96", fill = "white") +
  geom_sf(data = IntWsts, color = "#808B96", fill = "#EAECEE") +
  coord_sf(xlim = c(-121, -100), ylim = c(30, 50), expand = FALSE) +
  geom_point(data = boo, aes(x = LON_FS, y = LAT_FS, col = regen_glm),
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
               + DUFF_DEPTH
               + LITTER_DEPTH
               ,
               data = moo, family = binomial)
summary(mod.psme)
mod.psme.step <- stepAIC(mod.psme)
plotmo(mod.psme)
plotmo(mod.psme.step)


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








#################################################
# # 5-fold AUC. nb may fail if not all factor levels represented -- run again.
# data.glm<-data.glm[sample(nrow(data.glm)),]
# breaks <- 5
# folds <- cut(seq(1,nrow(data.glm)),breaks=breaks,labels=FALSE)
# AUC <- NULL
# for(i in 1:breaks){
#   foldIndexes <- which(folds==i,arr.ind=TRUE)
#   foldData <- data.glm[foldIndexes, ]
#   mod <- update(mod.glm, data = foldData)
#   foldPred <- predict(mod, foldData, type = "response")
#   roccurve <- roc(regen_glm ~ foldPred, data = foldData) 
#   area <- auc(roccurve)
#   AUC <- cbind(AUC, area)
# }
# (AUC.5fld.v1 <- rowMeans(AUC)) 
#################################################

