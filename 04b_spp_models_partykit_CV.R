## k-folds cross-validation - PIPO
#https://stackoverflow.com/questions/21380236/cross-validation-for-glm-models

# Rule of thumb for thresholding: prevalence of ones
data.pipo %>%
  count(regen_pipo)
# 0: 417
# 1: 104
# 104/417 = 0.2494005

# Randomly shuffle data
data<-data.pipo[sample(nrow(data.pipo)),]
# Create 10 equally size folds
folds <- cut(seq(1,nrow(data)),breaks=10,labels=FALSE)

acc <- vector()
auc <- vector()
for(i in 1:10){
  # Segement data by fold 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- data[testIndexes, ]
  trainData <- data[-testIndexes, ]
  
  ### CROSS VALIDATION WITH TRAINING DATA
  # Update model with train data
  mod <- update(glm.tree.pipo, data = trainData)
  
  # Use train model to predict test data
  predTest <- predict(mod, testData, type = "response")
  
  # Compute accuracy stats; set threshold for pred probabilities (0 or 1)
  # Rule of thumb for threshold: prevalence of ones
  cm <- confusionMatrix(data = as.factor(as.numeric(predTest>0.25)),
                        reference = as.factor(testData$regen_pipo)) 
  acc.temp <- cm$overall[1]
  acc <- cbind(acc, acc.temp)
  
  
  ### CROSS VALIDATION WITH AUC
  # Use fold (1/10 of data) to make new predictions
  predTest <- predict(glm.tree.pipo, testData, type = "response")
  # Generate curve and store AUC. Use same fold for comparison.
  roccurve <- roc(testData$regen_pipo ~ predTest)
  auc.temp <-  auc(roccurve) # Area under the curve.
  auc <- cbind(auc, auc.temp)
  }
(cv.acc <- rowMeans(acc)) #; rm(acc.temp, acc, cm)
(cv.auc <- rowMeans(auc)) #; rm(auc.temp, auc)





##########################################################
## k-folds cross-validation - PSME

# Rule of thumb for thresholding: prevalence of ones
data.psme %>%
  count(regen_psme)
# 0: 504
# 1: 202
# 202/504 = 0.4007937

# Randomly shuffle data
data<-data.psme[sample(nrow(data.psme)),]
# Create 10 equally size folds
folds <- cut(seq(1,nrow(data)),breaks=10,labels=FALSE)

acc <- vector()
auc <- vector()
for(i in 1:10){
  # Segement data by fold 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- data[testIndexes, ]
  trainData <- data[-testIndexes, ]
  
  ### CROSS VALIDATION WITH TRAINING DATA
  # Update model with train data
  mod <- update(glm.tree.psme, data = trainData)
  
  # Use train model to predict test data
  predTest <- predict(mod, testData, type = "response")
  
  # Compute accuracy stats; set threshold for pred probabilities (0 or 1)
  # Rule of thumb for threshold: prevalence of ones
  cm <- confusionMatrix(data = as.factor(as.numeric(predTest>0.40)),
                        reference = as.factor(testData$regen_psme)) 
  acc.temp <- cm$overall[1]
  acc <- cbind(acc, acc.temp)
  
  
  ### CROSS VALIDATION WITH AUC
  # Use fold (1/10 of data) to make new predictions
  predTest <- predict(glm.tree.psme, testData, type = "response")
  # Generate curve and store AUC
  roccurve <- roc(testData$regen_psme ~ predTest)
  auc.temp <-  auc(roccurve)
  auc <- cbind(auc, auc.temp)
}
(cv.acc <- rowMeans(acc)) #; rm(acc.temp, acc, cm)
(cv.auc <- rowMeans(auc)) #; rm(auc.temp, auc)


data.psme %>%
  count(regen_psme)
