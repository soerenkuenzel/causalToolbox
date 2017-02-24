# This file tests different implementations of the tree aglorithm in RF to make 
# sure that their performance is the same. 
# We use the following settings:
# mtry = 4
# sample all data points without replacement
# choose node size to be 1
# setwd("~/Dropbox/CATE/CausalRF/")
library(randomForest)
library(ranger)
library(ggplot2)
library(dplyr)

################################################################################
## Test 1: run each method several times and check that the MSE distribution is
## the same:

## test data:
iris$Species <- as.numeric(iris$Species)
set.seed(1312)
tetr <- sample(c('test', 'train'), size = nrow(iris), replace = TRUE)

iris_test <- iris[tetr == 'test', ]
iris_train <- iris[tetr == 'train', ]

nsim <- 1000
MSE_brei <- rep(NA, nsim)
MSE_ranger <- rep(NA, nsim)
MSE_hrt2 <- rep(NA, nsim)
MSE_hrt1 <- rep(NA, nsim)

#### Breiman and ranger implementation:
for(i in 1:nsim){
  t_brei <- randomForest(Sepal.Length ~ ., data=iris_train, 
                mtry = 4, 
                ntree = 1, 
                replace = FALSE, 
                sampsize = 1 * nrow(iris_train),
                nodesize = 1)
  t_ranger <- ranger(Sepal.Length ~ ., data=iris_train, write.forest = TRUE, 
                mtry = 4,
                num.trees = 1, 
                replace = FALSE,
                sample.fraction = 1,
                min.node.size = 1)
  
  predict_brei <- predict(t_brei, iris_test)
  predict_ranger <- predict(t_ranger, dat = iris_test)$predictions

  
  MSE_brei[i] <- mean((iris_test$Sepal.Length - predict_brei)^2)
  MSE_ranger[i] <- mean((iris_test$Sepal.Length - predict_ranger)^2)
}


#### hRF2 implementation:

source("hRF2/hTree.R")
source("hRF2/hRandomForest.R")

for(i in 1:nsim){
  start_time <- Sys.time()
  t_hrt2 <- htree(iris_train[ , -1], iris_train$Sepal.Length,
                iris_train[ , -1], iris_train$Sepal.Length,
                mtry = 4,
                minleafsize.J = 1, 
                minleafsize.I = 1)
  predict_hrt2 <- predict(t_hrt2, iris_test[ , -1])
  MSE_hrt2[i] <- mean((iris_test$Sepal.Length - predict_hrt2)^2)
  print(paste0("Training hRF2, done with simulation: ", i, 
               ", it took ", Sys.time() - start_time, "s"))
}

#### hRF1 implementation:

source("honestRF.R")
for(i in 1:nsim){
  start_time <- Sys.time()
  t_hrt1 <- buildHonestRF(xSplit=iris_train[ , -1], ySplit=iris_train$Sepal.Length, 
                          minSizeSplit=1, 
                          xAverage=iris_train[ , -1], yAverage=iris_train$Sepal.Length, 
                          minSizeAverage=1, nFeatures=4, maxDepth=NULL, 
                          nTrees=1, replace=TRUE)
  predict_hrt1 <- predictHonestRF(t_hrt1, iris_test[ , -1])
  MSE_hrt1[i] <- mean((iris_test$Sepal.Length - predict_hrt1)^2)
  print(paste0("Training hRF1, done with simulation: ", i, 
              ", it took ", Sys.time() - start_time, "s"))
}


data.frame(kind = rep(c('Breiman', 'ranger', 'hrt1', 'hrt2'), each = nsim),
           MSE = c(MSE_brei, MSE_ranger, MSE_hrt1, MSE_hrt2)) %>%
  ggplot() +
  geom_histogram(aes(x = MSE, fill = kind), bins = 10, position = 'dodge', 
                 alpha = 1)

# OK: Problem This should be determinisitic!


ggsave("Reports/figures/TestTreesEMSE.pdf", width = 13, height = 8)

data.frame(kind = rep(c('Breiman', 'ranger', 'hrt1', 'hrt2'), each = nsim),
           MSE = c(MSE_brei, MSE_ranger, MSE_hrt1, MSE_hrt2)) %>%
  ggplot() +
  geom_histogram(aes(x = MSE, fill = kind), bins = 20, position = 'dodge', 
                 alpha = .5)
ggsave("Reports/figures/TestTreesEMSE2.pdf", width = 13, height = 8)



