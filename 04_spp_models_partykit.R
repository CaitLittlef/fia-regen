######################################################
### SETUP
######################################################

currentDate <- Sys.Date()

## Load data; remove extraneous column if necessary
data.pipo <- read.csv("data.pipo.csv") ; data.pipo$X <- NULL
data.psme <- read.csv("data.psme.csv") ; data.psme$X <- NULL

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

######################################################
### PIPO GLM TREE
######################################################
## Grow tree
glm.tree.pipo <- glmtree(regen_pipo ~ YEAR.DIFF |
                  BALive_pipo +
                + def.tc
                + tmax.tc
                + ppt.tc
                + CMD_CHNG
                + def59_z_13
                # + ELEV # effectively taken care of with climatologies
                + REBURN
                + FIRE.SEV
                ,
                data = data.pipo,
                family = binomial(link = "logit"),
                minsplit = 50, 
                ordinal = "L2") 
plot(glm.tree.pipo)

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

## Also see _CV.R for alt ways: cv.glm, MSE, confusion matrices (w/ caret)

###########
## Proportional deviance explained (pseudo R^2). Ref: Zuur et al. 2009 p. 218
# Create null with just intercept; can't pull null deviance from glm tree only resid dev.
mod.pipo.null <- glm(regen_pipo ~ 1, data = data.pipo, family = binomial)
# Tree model over non-tree null.
(deviance(mod.pipo.null) - deviance(glm.tree.pipo))/deviance(mod.pipo.null)

## C.f., non tree (which ultimately means multiple models vs. 1)
# Non-tree full
mod.pipo = glm(regen_pipo ~ YEAR.DIFF
               + BALive_pipo 
               + def.tc
               + tmax.tc
               + ppt.tc
               + CMD_CHNG
               + def59_z_13
               # + ELEV # effectively taken care of with climatologies
               + REBURN
               + FIRE.SEV,
               data = data.pipo, family = binomial)
summary(mod.pipo)
# Non-tree only signif
mod.pipo.select <- update(mod.pipo, regen_pipo ~ YEAR.DIFF + BALive_pipo + FIRE.SEV)
summary(mod.pipo.select)
# Non-tree step
mod.pipo.step <- stepAIC(mod.pipo)
summary(mod.pipo.step) # Suggests dropping FIRE.SEV

# With likelihood ratio test (ok b/c they're nested), which is better? 
lrtest(mod.pipo.step, mod.pipo.select) # No diff (p > 0.05) so keep more parsimonious step.


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





########################################## PSME
## Grow tree
data.psme$FIRE.SEV <- factor(data.psme$FIRE.SEV, ordered = TRUE)
glm.tree.psme <- glmtree(regen_psme ~ YEAR.DIFF
                         | BALive_psme 
                         + def.tc
                         + tmax.tc
                         + ppt.tc
                         + CMD_CHNG
                         # + def59_z_1
                         # + def59_z_2
                         # + def59_z_3
                         + def59_z_13
                         # + ELEV # effectively taken care of with climatologies
                         + REBURN
                         + FIRE.SEV
                         ,
                         data = data.psme, 
                         family = binomial(link = "logit"),
                         minsplit = 100, # weights required for splits, not just number obs
                         ordinal = "L2") # Use dedicated ordinal stat for ordinal vars (FIRE.SEV)


## Output
plot(glm.tree.psme) ; glm.tree.psme
# Save plot
# tiff(paste0(out.dir,"psme_tree_",currentDate,".tiff"),
#      width = 640, height = 480, units = "px")
# plot(glm.tree.psme)
# dev.off()


##########################################################
## Model eval
data.psme$regen_psme <- as.numeric(as.character(data.psme$regen_psme)) # need numeric for c.f. pred

## Calculate deviance explained
# Non-tree null
mod.psme.null <- glm(regen_psme ~ 1, data = data.psme, family = binomial(link = "logit"))
# Non-tree year diff
mod.psme.yrdiff <- glm(regen_psme ~ YEAR.DIFF, data = data.psme, family = binomial(link = "logit"))
data.psme$FIRE.SEV <- as.numeric(data.psme$FIRE.SEV) # set for simple glm
data.psme$regen_psme <- as.numeric(as.character(data.psme$regen_psme)) # set for simple glm
# Non-tree full
mod.psme = glm(regen_psme ~ YEAR.DIFF
               + BALive_psme #+ BALiveTot
               + def.tc
               + tmax.tc
               + ppt.tc
               + CMD_CHNG
               + def59_z_13
               # + ELEV # effectively taken care of with climatologies
               + REBURN
               + FIRE.SEV,
               data = data.psme, family = binomial)
# Non-tree step
mod.psme.step <- stepAIC(mod.psme)
summary(mod.psme.step)
# Non-tree only signif
# ^ same as step

data.psme$FIRE.SEV <- factor(data.psme$FIRE.SEV, ordered = TRUE) # set back for glm tree
data.psme$regen_psme <- factor(data.psme$regen_psme, ordered = FALSE) # set back for glm tree
summary(mod.psme)
# Tree model over non-tree null.
(deviance(mod.psme.null) - deviance(glm.tree.psme))/deviance(mod.psme.null)
# Tree model over non-tree simple.
(deviance(mod.psme.yrdiff) - deviance(glm.tree.psme))/deviance(mod.psme.yrdiff)
# Tree model over non-tree full.
(deviance(mod.psme) - deviance(glm.tree.psme))/deviance(mod.psme)
# Tree model over non-tree step
(deviance(mod.psme.step) - deviance(glm.tree.psme))/deviance(mod.psme.step)
summary(mod.psme.step)
AIC(glm.tree.psme, mod.psme.null, mod.psme.yrdiff, mod.psme, mod.psme.step)


##########
## MSE
data.psme$regen_psme <- as.numeric(as.character(data.psme$regen_psme))
regen_pred <- predict(glm.tree.psme, newdata = data.psme, type ="response")
(MSE = mean((data.psme$regen_psme - regen_pred)^2))

###########
## ROC assesses discrimination w/ sensitivity (true pos) & specificity (true neg); want =1
regen_pred=predict(glm.tree.psme, newdata = data.psme, type ="response")
data.psme$regen_pred <- regen_pred
roccurve <- roc(regen_psme ~ regen_pred, data = data.psme)
par(mfrow=c(1,1))
plot(roccurve)
auc(roccurve)


###########
## Prediction rate on orig data (confusion matrix) 
regen_pred <- predict(glm.tree.psme, newdata = data.psme, type = "response") # gives decimals
thresh <- 0.5 # define threshold for categorizing predicted probabilities.
# Then cut predicted probabilities into categories 
predCat <- cut(regen_pred, breaks=c(-Inf, thresh, Inf), labels=c("0", "1"))
# Create confusion matrix
confusn <- table(predCat, data.psme$regen_psme, dnn=c("predicted", "actual"))
addmargins(confusn)


## Alt use caret package
regen_pred <- predict(glm.tree.psme, newdata = data.psme, type = "response") # gives decimals in probability
confusionMatrix(data = as.factor(as.numeric(regen_pred>0.5)), # needs factor but divide decimals into 0/1 first
                reference = as.factor(data.psme$regen_psme), # needs factor
                dnn = c("predicted", "actual"))


##
## Alt self-created hold-out (should ~ accuracy from k-folds cross-validation)
# Randomly shuffle data
data<-data.psme[sample(nrow(data.psme)),]
# Create 10 equally size folds
folds <- cut(seq(1,nrow(data)),breaks=10,labels=FALSE)
# Subset test and training data 
for(i in 1:10){
  #Segement data by fold 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- data[testIndexes, ]
  trainData <- data[-testIndexes, ]
}
# Train model 
glm.tree.psme.train <- glmtree(regen_psme ~ YEAR.DIFF
                               | BALive_psme +  MAP_1995 
                               + CMD_1995
                               + CMD_CHNG
                               + FIRE.SEV,
                               data = trainData, 
                               family = binomial(link = "logit"),
                               minsplit = 50, # weights required for splits, not just number obs
                               ordinal = "L2") # Use dedicated ordinal stat for ordinal vars (FIRE.SEV)

# Use trained model to predict testing data
predTest <- predict(glm.tree.psme.train, testData, type = "response")
thresh <- 0.5 # define threshold for categorizing predicted probabilities.
# Then cut predicted probabilities into categories 
predCat <- cut(predTest, breaks=c(-Inf, thresh, Inf), labels=c("0", "1"))
# Create confusion matrix
confusn <- table(predCat, testData$regen_psme, dnn=c("predicted", "actual"))
addmargins(confusn)

## Alt, use caret package
predTest <- predict(glm.tree.psme.train, testData)
confusionMatrix(data = as.factor(as.numeric(predTest>0.5)), # needs factor but set num for 0/1 first
                reference = as.factor(testData$regen_psme), # needs factor
                dnn = c("predicted", "actual"))


###########
## K-fold cross-validation prediction error -- NA for glm trees


##############################################
## Grab & plot observations in given node
## Get obs for each node (orig as df) and save row.names (match orig dataset)
# Create new col in df to assign node number
data.psme$NODE <- NA 
data.psme$NODE %>% factor()

# How many and what ID? Keep only those for assigning. length() gives all, width() gives terminal
diff <- (length(glm.tree.psme) - width(glm.tree.psme)) # Gives nodes that SHOULDN'T get assigned
loop.ready <- ((1+diff):(width(glm.tree.psme)+diff)) # gives node value that should get assigned

# Loop through all terminal nodes
for (i in loop.ready){ # length(summary) to only get terminal nodes
  node <- paste0("node",(i))
  obs <- glm.tree.psme[[i]]$data %>% row.names()
  data.psme$NODE[rownames(data.psme) %in% obs] <- paste0(node)
}

data.psme %>% group_by(NODE) %>% count()

p <- ggplot() +
  # state outlines
  geom_sf(data = Wsts) +
  geom_point(data = data.psme, aes(x = LON_FS, y = LAT_FS, col = NODE),
                                   size = 4, alpha = 0.7) +
  scale_color_manual(values = c("yellow", "orange", "light green", "dark green"),
                     labels = c("3: <BA <CMD", "4: <BA >CMD", "6: >BA <CMD", "7: >BA >CMD"))
p
# tiff(paste0(out.dir,"psme_tree_map_",currentDate,".tiff"),
#       width = 640, height = 480, units = "px")
# p
# dev.off()





########################################## PIED
# too few obs