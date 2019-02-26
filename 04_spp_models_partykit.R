currentDate <- Sys.Date()


## Load data; remove extraneous column if necessary
data.pied <- read.csv("data.pied_wwoburn.csv") ; data.pied$X <- NULL
data.pipo <- read.csv("data.pipo_wwoburn.csv") ; data.pipo$X <- NULL
data.psme <- read.csv("data.psme_wwoburn.csv") ; data.psme$X <- NULL


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

# Create CMD relative chng
data.pied$CMD_CHNG <- (data.pied$CMD_2025 - data.pied$CMD_1995)/data.pied$CMD_1995
data.pipo$CMD_CHNG <- (data.pipo$CMD_2025 - data.pipo$CMD_1995)/data.pipo$CMD_1995
data.psme$CMD_CHNG <- (data.psme$CMD_2025 - data.psme$CMD_1995)/data.psme$CMD_1995

################################################
## PIPO GLM TREE
# Excluding def59_z_0, _1, _2, _3, _03, _13, b/c not used. 
# But nb including 59z0 replaces FIRE.SEV -- maybe related to fire weather?
# More deficit and higher severity --> increased cumulative probability.
# Also excluding I(YEAR.DIFF) <- need I() to treat "as-is" given it's covariate in glm.
# See https://stats.stackexchange.com/questions/390014/default-plot-for-mob-object-glm-tree-not-returned-using-party-package



## Grow tree
glm.tree.pipo <- glmtree(regen_pipo ~ YEAR.DIFF
                | BALive_pipo +  MAP_1995 #+ CMD_1995 #+ BALiveTot
                # + I(YEAR.DIFF)
                # + REBURN
                + CMD_CHNG
                + FIRE.SEV,
                data = data.pipo, 
                family = binomial(link = "logit"),
                minsplit = 75, # weights required for splits, not just number obs
                ordinal = "L2") # Use dedicated ordinal stat for ordinal vars (FIRE.SEV)

## Output
plot(glm.tree.pipo) ; glm.tree.pipo
# plot(glm.tree.pipo, terminal_panel = NULL)
# summary(tree.mob.pipo) # Shows stats at each terminal node
# Save plot
# tiff(paste0(out.dir,"pipo_tree_",currentDate,".tiff"),
#      width = 640, height = 480, units = "px")
plot(glm.tree.pipo)
dev.off()


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
               + MAP_1995
               + CMD_CHNG
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
regen_pred <- predict(glm.tree.pipo, newdata = data.pipo, type = "response")
thresh <- 0.5 # define threshold for categorizing predicted probabilities.
# Then cut predicted probabilities into categories 
predCat <- cut(regen_pred, breaks=c(-Inf, thresh, Inf), labels=c("0", "1"))
# Create confusion matrix
confusn <- table(predCat, data.pipo$regen_pipo, dnn=c("predicted", "actual"))
addmargins(confusn)


## Alt use caret package
regen_pred <- predict(glm.tree.pipo, newdata = data.pipo, type = "response")
confusionMatrix(data = as.factor(as.numeric(regen_pred>0.5)), # needs factor but set num for 0/1 first
                reference = as.factor(data.pipo$regen_pipo), # needs factor
                dnn = c("predicted", "actual"))


##
## Alt self-created hold-out (should ~ accuracy from k-folds cross-validation)
# Randomly shuffle data
data<-data.pipo[sample(nrow(data.pipo)),]
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
glm.tree.pipo.train <- glmtree(regen_pipo ~ YEAR.DIFF
                         | BALive_pipo +  MAP_1995 
                         + CMD_CHNG
                         + FIRE.SEV,
                         data = trainData, 
                         family = binomial(link = "logit"),
                         minsplit = 75, # weights required for splits, not just number obs
                         ordinal = "L2") # Use dedicated ordinal stat for ordinal vars (FIRE.SEV)

# Use trained model to predict testing data
predTest <- predict(glm.tree.pipo.train, testData, type = "response")
thresh <- 0.5 # define threshold for categorizing predicted probabilities.
# Then cut predicted probabilities into categories 
predCat <- cut(predTest, breaks=c(-Inf, thresh, Inf), labels=c("0", "1"))
# Create confusion matrix
confusn <- table(predCat, testData$regen_pipo, dnn=c("predicted", "actual"))
addmargins(confusn)

## Alt, use caret package
predTest <- predict(glm.tree.pipo.train, testData)
confusionMatrix(data = as.factor(as.numeric(predTest>0.5)), # needs factor but set num for 0/1 first
                reference = as.factor(testData$regen_pipo), # needs factor
                dnn = c("predicted", "actual"))


###########
## K-fold cross-validation prediction error -- NA for glm trees






##############################################
## Grab & plot observations in given node
plot(glm.tree.pipo)
# Get obs for each node (orig as df) and save row.names (match orig dataset)
obs.node1 <- (glm.tree.pipo[[1]]$data) %>% row.names()
obs.node2 <- (glm.tree.pipo[[2]]$data) %>% row.names()
obs.node3 <- (glm.tree.pipo[[3]]$data) %>% row.names()
obs.node4 <- (glm.tree.pipo[[4]]$data) %>% row.names()
obs.node5 <- (glm.tree.pipo[[5]]$data) %>% row.names()
obs.node6 <- (glm.tree.pipo[[6]]$data) %>% row.names()
obs.node7 <- (glm.tree.pipo[[7]]$data) %>% row.names()

# Create new col in dataframe to assign node number
data.pipo$NODE <- NA 
data.pipo$NODE %>% factor()
# Assign NODE value -- need INDEX, not df
# data.pipo$NODE[data.pipo[obs.node5 ,]]
# ^ won't work b/c what's in brackets is a dataframe
# data.pipo$NODE[rownames(data.pipo) %in% obs.node5] <- "node5" 
# ^ works because what's in brackets is logical TRUE/FALSE -- an index.
# Nb b/c subsetting on column ($NODE) not df, don't need comma to say all cols (like here:) 

# Only assign terminal nodes
# data.pipo$NODE[rownames(data.pipo) %in% obs.node1] <- "node1"
data.pipo$NODE[rownames(data.pipo) %in% obs.node2] <- "node2"
# data.pipo$NODE[rownames(data.pipo) %in% obs.node3] <- "node3"
# data.pipo$NODE[rownames(data.pipo) %in% obs.node4] <- "node4"
data.pipo$NODE[rownames(data.pipo) %in% obs.node5] <- "node5"
data.pipo$NODE[rownames(data.pipo) %in% obs.node6] <- "node6"
data.pipo$NODE[rownames(data.pipo) %in% obs.node7] <- "node7"

data.pipo %>% group_by(NODE) %>% count()

p <- ggplot() +
  # state outlines
  geom_sf(data = Wsts) +
  geom_point(data = data.pipo, aes(x = LON_FS, y = LAT_FS, col = NODE)) +
  scale_color_manual(values = c("blue", "yellow", "orange", "dark green"),
                    labels = c("less chng CMD", "more chng CMD, less BA, low sev", "more chng CMD, less BA, high sev", "more chng CMD, more BA"))
p
# tiff(paste0(out.dir,"pipo_tree_map_",currentDate,".tiff"),
#       width = 640, height = 480, units = "px")
# p
dev.off()



########################################## PSME
## Grow tree
glm.tree.psme <- glmtree(regen_psme ~ YEAR.DIFF
                         | BALive_psme +  MAP_1995 #+ CMD_1995 #+ BALiveTot
                         # + I(YEAR.DIFF)
                         # + REBURN
                         + CMD_1995
                         + CMD_CHNG
                         + FIRE.SEV,
                         data = data.psme, 
                         family = binomial(link = "logit"),
                         minsplit = 75, # weights required for splits, not just number obs
                         ordinal = "L2") # Use dedicated ordinal stat for ordinal vars (FIRE.SEV)

## Output
plot(glm.tree.psme) ; glm.tree.psme
# Save plot
# tiff(paste0(out.dir,"psme_tree_",currentDate,".tiff"),
#      width = 640, height = 480, units = "px")
# plot(glm.tree.psme)
dev.off()


##############################################
## Grab & plot observations in given node
plot(glm.tree.psme)
# Get obs for each node (orig as df) and save row.names (match orig dataset)
obs.node1 <- (glm.tree.psme[[1]]$data) %>% row.names()
obs.node2 <- (glm.tree.psme[[2]]$data) %>% row.names()
obs.node3 <- (glm.tree.psme[[3]]$data) %>% row.names()
obs.node4 <- (glm.tree.psme[[4]]$data) %>% row.names()
obs.node5 <- (glm.tree.psme[[5]]$data) %>% row.names()
obs.node6 <- (glm.tree.psme[[6]]$data) %>% row.names()
obs.node7 <- (glm.tree.psme[[7]]$data) %>% row.names()

# Create new col in dataframe to assign node number
glm.tree.psme$NODE <- NA 
glm.tree.psme$NODE %>% factor()

# Only assign terminal nodes
# data.psme$NODE[rownames(data.psme) %in% obs.node1] <- "node1"
# data.psme$NODE[rownames(data.psme) %in% obs.node2] <- "node2"
data.psme$NODE[rownames(data.psme) %in% obs.node3] <- "node3"
data.psme$NODE[rownames(data.psme) %in% obs.node4] <- "node4"
# data.psme$NODE[rownames(data.psme) %in% obs.node5] <- "node5"
data.psme$NODE[rownames(data.psme) %in% obs.node6] <- "node6"
data.psme$NODE[rownames(data.psme) %in% obs.node7] <- "node7"

data.psme %>% group_by(NODE) %>% count()

p <- ggplot() +
  # state outlines
  geom_sf(data = Wsts) +
  geom_point(data = data.psme, aes(x = LON_FS, y = LAT_FS, col = NODE)) +
  scale_color_manual(values = c("blue", "yellow", "orange", "dark green"),
                     labels = c("less BA, smaller CMD", "less BA, bigger CMD", "more BA, smaller CMD", "more BA, bigger CMD"))
p
# tiff(paste0(out.dir,"psme_tree_map_",currentDate,".tiff"),
#       width = 640, height = 480, units = "px")
p
# dev.off()





########################################## PIED
# too few obs