currentDate <- Sys.Date()


## Load data; remove extraneous column if necessary
data.pied <- read.csv("data.pied.csv") ; data.pied$X <- NULL
data.pipo <- read.csv("data.pipo.csv") ; data.pipo$X <- NULL
data.psme <- read.csv("data.psme.csv") ; data.psme$X <- NULL


## Exclude sites w/o fire OR w/ fire.sev 5 & 6 (here NA)
data.pied <- data.pied[! is.na(data.pied$FIRE.SEV) ,]
data.pipo <- data.pipo[! is.na(data.pipo$FIRE.SEV) ,]
data.psme <- data.psme[! is.na(data.psme$FIRE.SEV) ,]


## Set variable classes -- viz of glm trees and signif p-thresholds for partitioning...
# ...are dependant on class (nb ordinal = L2 to use dedicated ordinal statistic...
# ...else requisite p-threshold is too low to capture FIRE.SEV).
# Each additional partitioning variable sets more stringent signif threshold.
# See https://stats.stackexchange.com/questions/392516/removal-of-partitioning-variable-in-final-glmtree-with-new-unused-partitioner/392682#392682
data.pied$regen_pied <- factor(data.pied$regen_pied, ordered = FALSE)
data.pipo$regen_pipo <- factor(data.pipo$regen_pipo, ordered = FALSE)
data.psme$regen_psme <- factor(data.psme$regen_psme, ordered = FALSE)
data.pied$FIRE.SEV <- factor(data.pied$FIRE.SEV, ordered = TRUE)
data.pipo$FIRE.SEV <- factor(data.pipo$FIRE.SEV, ordered = TRUE)
data.psme$FIRE.SEV <- factor(data.psme$FIRE.SEV, ordered = TRUE)
data.pied$REBURN <- factor(data.pied$REBURN, ordered = TRUE)
data.pipo$REBURN <- factor(data.pipo$REBURN, ordered = TRUE)
data.psme$REBURN <- factor(data.psme$REBURN, ordered = TRUE)

# Create CMD relative chng (from projected or observbed z-score slopes)
# data.pied$CMD_CHNG <- (data.pied$CMD_2025 - data.pied$CMD_1995)/data.pied$CMD_1995
# data.pipo$CMD_CHNG <- (data.pipo$CMD_2025 - data.pipo$CMD_1995)/data.pipo$CMD_1995
# data.psme$CMD_CHNG <- (data.psme$CMD_2025 - data.psme$CMD_1995)/data.psme$CMD_1995
data.pied$CMD_CHNG <- data.pied$def59.z.slope
data.pipo$CMD_CHNG <- data.pipo$def59.z.slope
data.psme$CMD_CHNG <- data.psme$def59.z.slope
# data.pied$CMD_CHNG <- data.pied$def59.z.slope84_17
# data.pipo$CMD_CHNG <- data.pipo$def59.z.slope84_17
# data.psme$CMD_CHNG <- data.psme$def59.z.slope84_17

data.pied$BAProp_pied <- data.pied$BALive_pied/data.pied$BALiveTot
data.pipo$BAProp_pipo <- data.pipo$BALive_pipo/data.pipo$BALiveTot
data.psme$BAProp_psme <- data.psme$BALive_psme/data.psme$BALiveTot


################################################
## PIPO GLM TREE
# TerraClimate data are not at exact coords, but ClimateWNA (e.g., MAP_1995) are wildly inaccurate.
# plot(data.pipo$MAP_1995, data.pipo$ppt.tc,
#      xlab = c("ClimateWNA precip"), ylab = c("TerraClimate precip"))
# abline(0,1)
# abline(v=900) # dropping out sites with MAP_1995 > 900 is when MAP_1995 is no longer partitioner (depending on number of other potential covariates included).
# So, using only TerraClimate, even though precip no longer pops in tree.
# Excluding def59_z_0, _1, _2, _3, _03, _13, b/c not used. 
# But nb including 59z0 replaces FIRE.SEV -- maybe related to fire weather?
# More deficit and higher severity --> increased cumulative probability.
# Excluding BALive_tot which eliminates other partitions: not adding new insight.
# Also excluding I(YEAR.DIFF) <- need I() to treat "as-is" given it's covariate in glm.
# See https://stats.stackexchange.com/questions/390014/default-plot-for-mob-object-glm-tree-not-returned-using-party-package


## Grow tree
# If want response curves not boxplot, set regen_pipo as numeric. 
glm.tree.pipo <- glmtree(regen_pipo ~ YEAR.DIFF |
                    BALive_pipo +
                # + BAProp_pipo
                # + def.tc
                # + tmax.tc
                # + ppt.tc
                # + CMD_CHNG
                # + def59_z_13
                + FIRE.SEV
                ,
                data = data.pipo,
                # data = data.pipo
                family = binomial(link = "logit"),
                minsplit = 50, # weights required for splits, not just number obs
                ordinal = "L2") # Use dedicated ordinal stat for ordinal vars (FIRE.SEV)
plot(glm.tree.pipo)


## Output
plot(glm.tree.pipo) ; glm.tree.pipo
# plot(glm.tree.pipo, terminal_panel = NULL)
# summary(glm.tree.pipo) # Shows stats at each terminal node
# Save plot
# tiff(paste0(out.dir,"pipo_tree_",currentDate,".tiff"),
#      width = 640, height = 480, units = "px")
# plot(glm.tree.pipo)
# dev.off()



##########################################################
## Model eval
data.pipo$regen_pipo <- as.numeric(as.character(data.pipo$regen_pipo)) # need numeric for c.f. pred

## Calculate deviance explained
mod.pipo.null <- glm(regen_pipo ~ 1, data = data.pipo, family = binomial(link = "logit"))
mod.pipo.yrdiff <- glm(regen_pipo ~ YEAR.DIFF, data = data.pipo, family = binomial(link = "logit"))
data.pipo$FIRE.SEV <- as.numeric(data.pipo$FIRE.SEV) # set for simple glm
data.pipo$regen_pipo <- as.numeric(as.character(data.pipo$regen_pipo)) # set for simple glm
mod.pipo = glm(regen_pipo ~ YEAR.DIFF
               + BALive_pipo #+ BALiveTot
               # + def.tc
               # + tmax.tc
               # + ppt.tc
               # + CMD_CHNG
               # + def59_z_13
               + FIRE.SEV,
               data = data.pipo, family = binomial(link = "logit"))
data.pipo$FIRE.SEV <- factor(data.pipo$FIRE.SEV, ordered = TRUE) # set back for glm tree
data.pipo$regen_pipo <- factor(data.pipo$regen_pipo, ordered = FALSE) # set back for glm tree
summary(mod.pipo)
# Tree model over non-tree null.
(deviance(mod.pipo.null) - deviance(glm.tree.pipo))/deviance(mod.pipo.null)
# Tree model over non-tree simple.
(deviance(mod.pipo.yrdiff) - deviance(glm.tree.pipo))/deviance(mod.pipo.yrdiff)
# Tree model over non-tree full.
(deviance(mod.pipo) - deviance(glm.tree.pipo))/deviance(mod.pipo)
# Minor improvement over full simple glm
summary(glm.tree.pipo)


###########
## MSE
data.pipo$regen_pipo <- as.numeric(as.character(data.pipo$regen_pipo))
regen_pred <- predict(glm.tree.pipo, newdata = data.pipo, type ="response")
(MSE = mean((data.pipo$regen_pipo - regen_pred)^2))

###########
## ROC assesses discrimination w/ sensitivity (true pos) & specificity (true neg); want =1
regen_pred=predict(glm.tree.pipo, newdata = data.pipo, type ="response")
data.pipo$regen_pred <- regen_pred
roccurve <- roc(regen_pipo ~ regen_pred, data = data.pipo)
par(mfrow=c(1,1))
plot(roccurve)
auc(roccurve)


###########
## Prediction rate on orig data (confusion matrix) 
regen_pred <- predict(glm.tree.pipo, newdata = data.pipo, type = "response") # gives decimals
thresh <- 0.25 # define threshold for categorizing predicted probabilities. Rule of thumb: prevalence of 1s
# Then cut predicted probabilities into categories 
predCat <- cut(regen_pred, breaks=c(-Inf, thresh, Inf), labels=c("0", "1"))
# Create confusion matrix
confusn <- table(predCat, data.pipo$regen_pipo, dnn=c("predicted", "actual"))
addmargins(confusn)


## Alt use caret package
regen_pred <- predict(glm.tree.pipo, newdata = data.pipo, type = "response") # gives decimals in probability
confusionMatrix(data = as.factor(as.numeric(regen_pred>0.25)), # needs factor but divide decimals into 0/1 first
                reference = as.factor(data.pipo$regen_pipo), # needs factor
                dnn = c("predicted", "actual"))


## Alt self-created hold-out: basically k-folds CV (see CV.R); can't apply glm.cv()






##############################################
## Grab & plot observations in given node
## Get obs for each node (orig as df) and save row.names (match orig dataset)
# Create new col in df to assign node number
data.pipo$NODE <- NA 
data.pipo$NODE %>% factor()

# How many and what ID? Keep only those for assigning. length() gives all, width() gives terminal
diff <- (length(glm.tree.pipo) - width(glm.tree.pipo)) # Gives nodes that SHOULDN'T get assigned
loop.ready <- ((1+diff):(width(glm.tree.pipo)+diff)) # gives node value that should get assigned

# Loop through all terminal nodes
for (i in loop.ready){ # length(summary) to only get terminal nodes
  node <- paste0("node",(i))
  obs <- glm.tree.pipo[[i]]$data %>% row.names()
  data.pipo$NODE[rownames(data.pipo) %in% obs] <- paste0(node)
}

data.pipo %>% group_by(NODE) %>% count()

p <- ggplot() +
  # state outlines
  geom_sf(data = Wsts) +
  geom_point(data = data.pipo, aes(x = LON_FS, y = LAT_FS, col = NODE),
             size = 4, alpha = 0.7) +
  scale_color_manual(values = c("yellow", "orange", "light green", "dark green"),
                    labels = c("3: <BA <SEV", "4: <BA >SEV",
                               "5: >BA"))
p
# tiff(paste0(out.dir,"pipo_tree_map_",currentDate,".tiff"),
#       width = 640, height = 480, units = "px")
# p
# dev.off()



########################################## PSME
## Grow tree
glm.tree.psme <- glmtree(regen_psme ~ YEAR.DIFF
                         | BALive_psme 
                         # + CMD_CHNG
                         + def.tc
                         # + tmax.tc
                         # + ppt.tc
                         # + FIRE.SEV,
                         ,
                         data = data.psme, 
                         family = binomial(link = "logit"),
                         minsplit = 50, # weights required for splits, not just number obs
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
mod.psme.null <- glm(regen_psme ~ 1, data = data.psme, family = binomial(link = "logit"))
mod.psme.yrdiff <- glm(regen_psme ~ YEAR.DIFF, data = data.psme, family = binomial(link = "logit"))
data.psme$FIRE.SEV <- as.numeric(data.psme$FIRE.SEV) # set for simple glm
data.psme$regen_psme <- as.numeric(as.character(data.psme$regen_psme)) # set for simple glm
mod.psme = glm(regen_psme ~ YEAR.DIFF
               + BALive_psme 
               # + CMD_CHNG
               + def.tc,
               # + tmax.tc
               # + ppt.tc
               # + FIRE.SEV,
               data = data.psme, family = binomial(link = "logit"))
data.psme$FIRE.SEV <- factor(data.psme$FIRE.SEV, ordered = TRUE) # set back for glm tree
data.psme$regen_psme <- factor(data.psme$regen_psme, ordered = FALSE) # set back for glm tree
summary(mod.psme)
# Tree model over non-tree null.
(deviance(mod.psme.null) - deviance(glm.tree.psme))/deviance(mod.psme.null)
# Tree model over non-tree simple.
(deviance(mod.psme.yrdiff) - deviance(glm.tree.psme))/deviance(mod.psme.yrdiff)
# Tree model over non-tree full.
(deviance(mod.psme) - deviance(glm.tree.psme))/deviance(mod.psme)
# Minor improvement over full simple glm


###########
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
                               minsplit = 75, # weights required for splits, not just number obs
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