## k-folds cross-validation
#https://stackoverflow.com/questions/21380236/cross-validation-for-glm-models

## Recall data.brt is either data.pipo or data.psme; Use regen_brt & BALive_brt.

# Rule of thumb for thresholding: prevalence of ones
data.brt %>%
  dplyr::count(regen_brt)
# 0: 417
# 1: 104
# 104/417 = 0.2494005

# Randomly shuffle data
data<-data.brt[sample(nrow(data.brt)),]
# Create 10 equally size folds
folds <- cut(seq(1,nrow(data)),breaks=10,labels=FALSE)

auc <- vector()
for(j in 1:length(models)){ # for each model
  for(i in 1:10){ # number of folds
  
  ### SEGMENT DATA BY FOLD
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- data[testIndexes, ]
  trainData <- data[-testIndexes, ]
  
  ### CROSS VALIDATION WITH AUC
  # Use fold (1/10 of data) to make new predictions
  n.trees <- models[[i]]$trees.fitted[match(TRUE, models[[i]]$cv.values == min(models[[i]]$cv.values))]
  predTest <- predict.gbm(models[[i]], n.trees = n.trees, testData, type = "response")
  # Generate curve and store AUC. Use same fold for comparison.
  roccurve <- roc(testData$regen_brt ~ predTest)
  auc.temp <-  pROC::auc(roccurve)
  auc <- cbind(auc, auc.temp)
  }
  }
(cv.auc <- rowMeans(auc)) #; rm(auc.temp, auc)
