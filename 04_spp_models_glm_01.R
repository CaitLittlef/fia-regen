currentDate <- Sys.Date()


## Load data; remove extraneous column if necessary
data.pied <- read.csv("data.pied_wwoburn.csv") ; data.pied$X <- NULL
data.pipo <- read.csv("data.pipo_wwoburn.csv") ; data.pipo$X <- NULL
data.psme <- read.csv("data.psme_wwoburn.csv") ; data.psme$X <- NULL


## Exclude sites w/o fire OR w/ fire.sev 5 & 6 (here NA)
data.pied <- data.pied[! is.na(data.pied$FIRE.SEV) ,]
data.pipo <- data.pipo[! is.na(data.pipo$FIRE.SEV) ,]
data.psme <- data.psme[! is.na(data.psme$FIRE.SEV) ,]

## Set variable classes
# data.pied$regen_pied <- factor(data.pied$regen_pied, ordered = FALSE)
# data.pipo$regen_pipo <- factor(data.pipo$regen_pipo, ordered = FALSE)
# data.psme$regen_psme <- factor(data.psme$regen_psme, ordered = FALSE)
data.pied$regen_pied <- as.numeric(as.character(data.pied$regen_pied)) # else gives factor level position
data.pipo$regen_pipo <- as.numeric(as.character(data.pipo$regen_pipo)) # else gives factor level position
data.psme$regen_psme <- as.numeric(as.character(data.psme$regen_psme)) # else gives factor level position
# data.pied$FIRE.SEV <- factor(data.pied$FIRE.SEV, ordered = TRUE)
# data.pipo$FIRE.SEV <- factor(data.pipo$FIRE.SEV, ordered = TRUE)
# data.psme$FIRE.SEV <- factor(data.psme$FIRE.SEV, ordered = TRUE)
data.pied$FIRE.SEV <- as.numeric(data.pied$FIRE.SEV) # else generates diff var for each factor level
data.pipo$FIRE.SEV <- as.numeric(data.pipo$FIRE.SEV) # else generates diff var for each factor level
data.psme$FIRE.SEV <- as.numeric(data.psme$FIRE.SEV) # else generates diff var for each factor level
data.pied$REBURN <- factor(data.pied$REBURN, ordered = TRUE)
data.pipo$REBURN <- factor(data.pipo$REBURN, ordered = TRUE)
data.psme$REBURN <- factor(data.psme$REBURN, ordered = TRUE)

# Create CMD relative chng
data.pied$CMD_CHNG <- (data.pied$CMD_2025 - data.pied$CMD_1995)/data.pied$CMD_1995
data.pipo$CMD_CHNG <- (data.pipo$CMD_2025 - data.pipo$CMD_1995)/data.pipo$CMD_1995
data.psme$CMD_CHNG <- (data.psme$CMD_2025 - data.psme$CMD_1995)/data.psme$CMD_1995

##########################################################
## glm of pipo regen
# n.b., adding z-scores of deficit do nothing. EMT, EXT, RH, SHM (summer heat metric) showed some promise, but didn't improve
# z-scores of deficit do not improve either.
mod.pipo = glm(regen_pipo ~ YEAR.DIFF
               + BALive_pipo #+ BALiveTot
               + MAP_1995
               + CMD_CHNG
               + FIRE.SEV,
               data = data.pipo, family = binomial(link = "logit"))
summary(mod.pipo)  
# plot(mod.pipo)


##########################################################
## Model eval
# 1) Goodness-of-fit -- maybe aka deviance/calibration
# 2) Skill -- maybe aka predictive ability/discrimination
# a) RMSE works if error is distrib norm (not here with binary outcomes)
# b) Percent predicted correctly (confusion) -- may show up as MSE accuracy rate
# c) ROC
## Understand predictions: if there's a link function relating linear predictor to expected value...
# ...predict gives fitted values BEFORE inverse of link function and fitted gives after link applied.
# log(fitted(mod.pipo)[5]) ; predict(mod.pipo)[5] # ~ roughly the same as log(fitted())
## With predict(), default ("link") is on scale of linear predictors, "response" gives scale of response var.
# So default predictions are of log-odds while "response" gives predicted probabilities.

###########
## Deviance (badness)
mod.pipo.null <- glm(regen_pipo ~ 1, data = data.pipo, family = binomial(link = "logit"))
mod.pipo.yrdiff <- glm(regen_pipo ~ YEAR.DIFF, data = data.pipo, family = binomial(link = "logit"))
(deviance(mod.pipo.null) - deviance(mod.pipo))/deviance(mod.pipo.null) # 0.08
(deviance(mod.pipo.null) - deviance(mod.pipo.yrdiff))/deviance(mod.pipo.null) # 0.04 

## Would glm tree improve deviance? 
data.pipo$FIRE.SEV <- factor(data.pipo$FIRE.SEV, ordered = TRUE) # set for glm tree
mod.pipo.tree <- glmtree(regen_pipo ~ YEAR.DIFF
                         + BALive_pipo
                         + MAP_1995
                         + CMD_CHNG
                         + FIRE.SEV
                         | BALive_pipo
                         + MAP_1995
                         + CMD_CHNG
                         + FIRE.SEV,
                         data = data.pipo, 
                         family = binomial(link = "logit"),
                         minsplit = 75, # weights required for splits, not just number obs
                         ordinal = "L2") # Use dedicated ordinal stat for ordinal vars (FIRE.SEV)
plot(mod.pipo.tree) ; summary(mod.pipo.tree) ; dev.off() # no change from simple glm even tho tiny dev chng
(deviance(mod.pipo) - deviance(mod.pipo.tree))/deviance(mod.pipo) # tiny chng 0.001 prob due to factor vs. numeric FIRE.SEV
data.pipo$FIRE.SEV <- as.numeric(data.pipo$FIRE.SEV) # set-back for simple glm
# Nope -- not full glm tree (but see simplified one at 04_spp_models_partykit.R)

###########
## MSE
regen_pred=predict(mod.pipo, type ="response")
(MSE = mean((data.pipo$regen_pipo - regen_pred)^2))

###########
## ROC assesses discrimination w/ sensitivity (true pos) & specificity (true neg); want =1
regen_pred=predict(mod.pipo, type ="response")
data.pipo$regen_pred <- regen_pred
roccurve <- roc(regen_pipo ~ regen_pred, data = data.pipo)
par(mfrow=c(1,1))
plot(roccurve)
auc(roccurve)

###########
## Prediction rate on orig data (confusion matrix) 
regen_pred <- predict(mod.pipo, type = "response")
thresh <- 0.5 # define threshold for categorizing predicted probabilities.
# Then cut predicted probabilities into categories 
predCat <- cut(regen_pred, breaks=c(-Inf, thresh, Inf), labels=c("0", "1"))
# Create confusion matrix
confusn <- table(predCat, data.pipo$regen_pipo, dnn=c("predicted", "actual"))
addmargins(confusn)

## Alt use caret package
regen_pred <- predict(mod.pipo, type = "response")
confusionMatrix(data = as.factor(as.numeric(regen_pred>0.5)), # needs factor but set num for 0/1 first
                reference = as.factor(data.pipo$regen_pipo), # needs factor
                dnn = c("predicted", "actual"))


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
mod.pipo.train = glm(regen_pipo ~ YEAR.DIFF
               + BALive_pipo #+ BALiveTot
               + MAP_1995 + CMD_CHNG
               + FIRE.SEV,
               data = trainData, family = binomial(link = "logit"))
# Use trained model to predict testing data
predTest <- predict(mod.pipo.train, testData, type = "response")
thresh <- 0.5 # define threshold for categorizing predicted probabilities.
# Then cut predicted probabilities into categories 
predCat <- cut(predTest, breaks=c(-Inf, thresh, Inf), labels=c("0", "1"))
# Create confusion matrix
confusn <- table(predCat, testData$regen_pipo, dnn=c("predicted", "actual"))
addmargins(confusn)
#          actual
# predicted  0  1 Sum
#       0   38 14  52
#       1    2  0   2
#       Sum 40 14  54

## Alt, use caret package
predTest <- predict(mod.pipo.train, testData)
confusionMatrix(data = as.factor(as.numeric(predTest>0.5)), # needs factor but set num for 0/1 first
                reference = as.factor(testData$regen_pipo), # needs factor
                dnn = c("predicted", "actual"))


###########
## K-fold cross-validation prediction error 
# Low k more biased (towards validation set) but high k suffers from lrg variability (towards leave-one-out cross-validation).
# cv.glm in binary classification is estimate of classification error rate on test set (avg's classification error rate on K validatio sets)
cv.glm(data.pipo, mod.pipo, K=10)$delta # 10-fold cross-validation: raw & adjusted; nb no K sets K=n so is basically leave-one-out
# 1 - 0.15 = 0.85% accuracy 

# The first component of delta is the average mean-squared error that you obtain from doing K-fold C.  
# The second component of delta is the average mean-squared error that you obtain from doing K-fold CV, but with a bias correction. How this is achieved is, initially, the residual sum of squares (RSS) is computed based on the GLM predicted values and the actual response values for the entire data set. As you're going through the K folds, you generate a training model, and then you compute the RSS between the entire data set of y-values (not just the training set) and the predicted values from the training model. These resulting RSS values are then subtracted from the initial RSS. After you're done going through your K folds, you will have subtracted K values from the initial RSS. This is the second component of delta.








##############################################################################
# # Are there other variables to consider? These are stand/fire plus all ClimateWNA
# f1 <- as.formula(paste("regen_pipo~",
#                        paste(names(data.pipo)[c(5,41:46,49,249,250:276,391,404,417,508)],
#                                             collapse = "+")))
# mod.all <- glm(f1, data = data.pipo, family = binomial(link = "logit"))
# summary(mod.all)
# # Maybe include EMT_1995 (extreme min temp) & EXT_1995 (extreme max temp) & RH_1995 & SHM_1995
# 
# 
# # Try adding those 3
# mod.pipo = glm(regen_pipo ~ YEAR.DIFF + BALive_pipo + BALiveTot + CMD_1995 + MAP_1995 + EMT_1995 + EXT_1995 + RH_1995 + SHM_1995 + FIRE.SEV, data = data.pipo, family = binomial(link = "logit"))
# summary(mod.pipo)
# # Does not much at all. Stick with original hunches