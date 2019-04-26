######################################################
### SETUP
######################################################

currentDate <- Sys.Date()

## Load data; remove extraneous column if necessary
data.pipo <- read.csv("data.pipo_2019-04-24.csv") ; data.pipo$X <- NULL
data.psme <- read.csv("data.psme_2019-04-24.csv") ; data.psme$X <- NULL

## Exclude sites w/o fire OR w/ fire.sev 5 & 6 (here NA)
data.pipo <- data.pipo[! is.na(data.pipo$FIRE.SEV) ,]
data.psme <- data.psme[! is.na(data.psme$FIRE.SEV) ,]

## Set variable classes
# data.pipo$regen_pipo <- factor(data.pipo$regen_pipo, ordered = FALSE)
data.pipo$regen_pipo <- as.numeric(as.character(data.pipo$regen_pipo)) 
# data.psme$regen_psme <- factor(data.psme$regen_psme, ordered = FALSE)
data.psme$regen_psmeo <- as.numeric(as.character(data.psme$regen_psme)) 
data.pipo$FIRE.SEV <- factor(data.pipo$FIRE.SEV, ordered = TRUE)
data.psme$FIRE.SEV <- factor(data.psme$FIRE.SEV, ordered = TRUE)
data.pipo$REBURN <- factor(data.pipo$REBURN, ordered = TRUE)
data.psme$REBURN <- factor(data.psme$REBURN, ordered = TRUE)

# Create CMD relative chng (from observed z-score slopes)
data.pipo$CMD_CHNG <- data.pipo$def59.z.slope
data.psme$CMD_CHNG <- data.psme$def59.z.slope

# Create proportion BA
data.pipo$BAProp_pipo <- data.pipo$BALive_pipo/data.pipo$BALiveTot
data.psme$BAProp_psme <- data.psme$BALive_psme/data.psme$BALiveTot


######################################################
### GLM TREE NOTES
######################################################

# Viz and signif thresholds for partitioning depend on var classes.
# E.g., if want curves not boxplot, set regen_pipo as numeric.
# Use dedicated ordinal statistic ordinal = L2 (else FIRE.SEV dropped).
# Each additional partitioning variable sets more stringent sifnif threshold
# Ref: https://stats.stackexchange.com/questions/392516/removal-of-partitioning-variable-in-final-glmtree-with-new-unused-partitioner/392682#392682 
# May need I(YEAR.DIFF) to treat var "as-is" if using covariate twice.
# Ref: https://stats.stackexchange.com/questions/390014/default-plot-for-mob-object-glm-tree-not-returned-using-party-package
# minsplit is f(number obs * weight). Here, w/ weights = 1, equals min number obs in node. 
# Tree growth is based on statistical stopping rules, so pruning not required (?).

######################################################
### PIPO GLM TREE
######################################################
## Grow tree
glm.tree.pipo <- glmtree(regen_pipo ~ YEAR.DIFF |
                  BALive_pipo + BALiveTot
                + def.tc
                + tmax.tc
                + ppt.tc
                + CMD_CHNG 
                # + def59_z_1
                # + def59_z_12
                # + def59_z_13
                # + def59_z_14
                # + def59_z_15
                # + ELEV
                + REBURN 
                + FIRE.SEV
                ,
                data = data.pipo,#[data.pipo$YEAR.DIFF > 3,],
                # data = data.pipo[data.pipo$BALive_pipo > 0,],
                # data = data.pipo[data.pipo$TRTCD1 == 0,],
                # data = data,
                family = binomial(link = "logit"),
                minsplit = 50, 
                ordinal = "L2") 
plot(glm.tree.pipo)

# z15 shows up when included, 
# but that makes little sense as many sites weren't sampled > 1 yr post-fire
data.pipo %>% count(def59_z_15 < -0.032, YEAR.DIFF <5)
hist(data.pipo$YEAR.DIFF[data.pipo$def59_z_15 < -0.032], 30)
# tiff(paste0(out.dir, "pipo_yr_diff_low_def_",currentDate,".tiff"),
#      width = 640, height = 480, units = "px")
# hist(data.pipo$YEAR.DIFF[data.pipo$def59_z_15 < -0.032], 30)
# dev.off()

## Output
plot(glm.tree.pipo) ; glm.tree.pipo # plot w/ terminal_panel = NULL shows stats
# summary(glm.tree.pipo) # Shows stats at each terminal node
# Save plot
# tiff(paste0(out.dir,"pipo_tree_",currentDate,".tiff"),
#      width = 640, height = 480, units = "px")
# plot(glm.tree.pipo)
# dev.off()


######################################################
### PIPO GLM TREE EVAL
######################################################

###########
## Proportional deviance explained (pseudo R^2). Ref: Zuur et al. 2009 p. 218
# Create null with just intercept; can't pull null deviance from glm tree only resid dev.
mod.pipo.null <- glm(regen_pipo ~ 1, data = data.pipo, family = binomial)
# Tree model over non-tree null.
(deviance(mod.pipo.null) - deviance(glm.tree.pipo))/deviance(mod.pipo.null)

## C.f., non tree (which ultimately means multiple models vs. 1)
# Non-tree full
mod.pipo = glm(regen_pipo ~ YEAR.DIFF
               + BALive_pipo + BALiveTot
               + def.tc
               + tmax.tc
               + ppt.tc
               + CMD_CHNG 
               # + def59_z_1
               # + def59_z_12
               # + def59_z_13
               # + def59_z_14
               # + def59_z_15
               # + ELEV
               + REBURN 
               + FIRE.SEV
               ,
               data = data.pipo, family = binomial)
summary(mod.pipo)
# Non-tree only signif
mod.pipo.select <- update(mod.pipo, regen_pipo ~ YEAR.DIFF + BALive_pipo + FIRE.SEV)
summary(mod.pipo.select)
# Non-tree step
mod.pipo.step <- stepAIC(mod.pipo)
summary(mod.pipo.step) # Suggests dropping FIRE.SEV

# With likelihood ratio test (ok b/c they're nested), which is better? 
lrtest(mod.pipo.step, mod.pipo.select) # Diff (p > 0.05) so keep select.


###########
## AIC
AIC(glm.tree.pipo, mod.pipo.step, mod.pipo.select)


###########
## 10-fold accuracy (from _CV.R script).
# Should ~ caret accuracy; for non-trees, use cv.glm
# tree: 0.6804333 
# non-tree: 0.715898


###########
## 10-fold AUC (from _CV.R script)
# tree: 0.7126641
# non-tree: 0.6763996


# ###########
# ## MSE
# data.pipo$regen_pipo <- as.numeric(as.character(data.pipo$regen_pipo))
# regen_pred <- predict(glm.tree.pipo, newdata = data.pipo, type ="response")
# (MSE = mean((data.pipo$regen_pipo - regen_pred)^2))
# 
# 
# ###########
# ## Confusion matrices (but cross-validation better)
# regen_pred <- predict(glm.tree.pipo, newdata = data.pipo, type = "response") # gives decimals
# thresh <- 0.25 # define threshold for categorizing predicted probabilities. Rule of thumb: prevalence of 1s
# # Then cut predicted probabilities into categories 
# predCat <- cut(regen_pred, breaks=c(-Inf, thresh, Inf), labels=c("0", "1"))
# # Create confusion matrix
# confusn <- table(predCat, data.pipo$regen_pipo, dnn=c("predicted", "actual"))
# addmargins(confusn)
# 
# ## Alt use caret package
# regen_pred <- predict(glm.tree.pipo, newdata = data.pipo, type = "response") # gives decimals in probability
# confusionMatrix(data = as.factor(as.numeric(regen_pred>0.25)), # needs factor but divide decimals into 0/1 first
#                 reference = as.factor(data.pipo$regen_pipo), # needs factor
#                 dnn = c("predicted", "actual"))



######################################################
### PSME GLM TREE
######################################################
glm.tree.psme <- glmtree(regen_psme ~ YEAR.DIFF 
                         | BALive_psme + BALiveTot
                         + def.tc
                         + tmax.tc
                         + ppt.tc
                         + CMD_CHNG 
                         # + def59_z_1
                         # + def59_z_12
                         # + def59_z_13
                         # + def59_z_14
                         # + def59_z_15
                         # + ELEV
                         + REBURN 
                         + FIRE.SEV
                         ,
                         data = data.psme,
                         # data = data.psme[data.psme$TRTCD1 == 0 ,],
                         family = binomial,
                         minsplit = 40, 
                         ordinal = "L2") 

## Output
plot(glm.tree.psme) ; glm.tree.psme
  # Save plot
# tiff(paste0(out.dir,"psme_tree_",currentDate,".tiff"),
#      width = 640, height = 480, units = "px")
# plot(glm.tree.psme)
# dev.off()



######################################################
### PSME GLM TREE EVAL
######################################################

###########
## Proportional deviance explained (pseudo R^2). Ref: Zuur et al. 2009 p. 218
# Create null with just intercept; can't pull null deviance from glm tree only resid dev.
mod.psme.null <- glm(regen_psme ~ 1, data = data.psme, family = binomial)
# Tree model over non-tree null.
(deviance(mod.psme.null) - deviance(glm.tree.psme))/deviance(mod.psme.null)

## C.f., non tree (which ultimately means multiple models vs. 1)
# Non-tree full
mod.psme = glm(regen_psme ~ YEAR.DIFF
               + BALive_psme + BALiveTot
               + def.tc
               + tmax.tc
               + ppt.tc
               + CMD_CHNG 
               # + def59_z_1
               # + def59_z_12
               # + def59_z_13
               # + def59_z_14
               # + def59_z_15
               # + ELEV
               + REBURN 
               + FIRE.SEV
               ,
               data = data.psme, family = binomial)
summary(mod.psme)
# Non-tree only signif
mod.psme.select <- update(mod.psme, regen_psme ~ YEAR.DIFF + BALive_psme + def.tc + ppt.tc + FIRE.SEV)
summary(mod.psme.select)
# Non-tree step
mod.psme.step <- stepAIC(mod.psme)
summary(mod.psme.step) # Suggests retaining precip, too. Identical.

# With likelihood ratio test (ok b/c they're nested), which is better? 
lrtest(mod.psme.step, mod.psme.select) # Identical


###########
## AIC
AIC(glm.tree.psme, mod.psme.step, mod.psme.select)


###########
## 10-fold accuracy (from _CV.R script).
# Should ~ caret accuracy; for non-trees, use cv.glm
# tree: 0.7110463 
# non-tree: 0.728169


###########
## 10-fold AUC (from _CV.R script)
# tree: 0.7949143
# non-tree: 0.7681333



# ###########
# ## MSE
# data.psme$regen_psme <- as.numeric(as.character(data.psme$regen_psme))
# regen_pred <- predict(glm.tree.psme, newdata = data.psme, type ="response")
# (MSE = mean((data.psme$regen_psme - regen_pred)^2))
# 
# 
# ###########
# ## Confusion matrices (but cross-validation better)
# regen_pred <- predict(glm.tree.psme, newdata = data.psme, type = "response") # gives decimals
# thresh <- 0.25 # define threshold for categorizing predicted probabilities. Rule of thumb: prevalence of 1s
# # Then cut predicted probabilities into categories 
# predCat <- cut(regen_pred, breaks=c(-Inf, thresh, Inf), labels=c("0", "1"))
# # Create confusion matrix
# confusn <- table(predCat, data.psme$regen_psme, dnn=c("predicted", "actual"))
# addmargins(confusn)
# 
# ## Alt use caret package
# regen_pred <- predict(glm.tree.psme, newdata = data.psme, type = "response") # gives decimals in probability
# confusionMatrix(data = as.factor(as.numeric(regen_pred>0.25)), # needs factor but divide decimals into 0/1 first
#                 reference = as.factor(data.psme$regen_psme), # needs factor
#                 dnn = c("predicted", "actual"))
