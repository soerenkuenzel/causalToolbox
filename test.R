##### Testing on Boston Housing dataset
library(MASS)
df <- Boston

# Take 20% out as validation set
val_idx <- sample(2, nrow(df), replace = TRUE, prob=c(0.8, 0.2))
xTrain <- df[val_idx == 1, -14]
yTrain <- df[val_idx == 1, 14]
xTest <- df[val_idx == 2, -14]
yTest <- df[val_idx == 2, 14]

# Generate splitting dataset v.s. averaging dataset (60:40)
split_idx <- sample(2, nrow(xTrain), replace = TRUE, prob=c(0.6, 0.4))
xSplit <- xTrain[split_idx == 1, ]
ySplit <- yTrain[split_idx == 1]
xAverage <- xTrain[split_idx == 2, ]
yAverage <- yTrain[split_idx == 2]

##### Train honestRF 
source("honestRF.R")
set.seed(24750371)
honestRFModel <- buildHonestRF(xSplit=xSplit, ySplit=ySplit, minSizeSplit=5, 
                               xAverage=xAverage, yAverage=yAverage, 
                               minSizeAverage=5, nFeatures=4, maxDepth=4, 
                               nTrees=100, replace=TRUE)
# Validate
yPredictedHRF <- predictHonestRF(honestRFModel, xTest)
print(mean((yPredictedHRF - yTest)^2))

###### Compare honestRF with RF
# Train a pseudo Random Forest using honestRF
set.seed(24750371)
honestRFModel_RF <- buildHonestRF(xSplit=xTrain, ySplit=yTrain, minSizeSplit=5, 
                                  xAverage=xTrain, yAverage=yTrain, 
                                  minSizeAverage=5, nFeatures=4, maxDepth=4, 
                                  nTrees=100, replace=TRUE)
# Validate
yPredictedHRFRF <- predictHonestRF(honestRFModel_RF, xTest)
print(mean((yPredictedHRFRF - yTest)^2))

# Test against regular RF implementation
set.seed(24750371)
#install.packages("randomForest")
library(randomForest)
RFModel <- randomForest(x=xTrain, y=yTrain, mtry=4, replace=TRUE, 
                        nodesize=5, maxnodes=2^4, ntree=100)
# Validate
yPredictedRF <- predict(RFModel, newdata=xTest)
print(mean((yPredictedRF - yTest)^2))
