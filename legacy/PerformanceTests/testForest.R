# This file tests different implementations of RF to make sure that their 
# performance is the same. The problem is that due to the randomization, 
# there always exist differences in the individual trees:
# setwd("~/Dropbox/CATE/CausalRF/")
library(randomForest)
library(ranger)
library(ggplot2)
library(dplyr)


################################################################################
## Test 1: run each method several times and check that the MSE distribution is
## the same:
iris$Species <- as.numeric(iris$Species)
set.seed(131)
tetr <- sample(c('test', 'train'), size = nrow(iris), replace = TRUE)

iris_test <- iris[tetr == 'test', ]
iris_train <- iris[tetr == 'train', ]

nsim <- 100
MSE_brei <- numeric(nsim)
MSE_ranger <- numeric(nsim)
for(i in 1:nsim){
  rf_brei <- randomForest(Sepal.Length ~ ., data=iris_train, 
                      mtry = 2, 
                      ntree = 500, 
                      replace = FALSE, 
                      sampsize = .5 * nrow(iris_train),
                      nodesize = 1)
  rf_ranger <- ranger(Sepal.Length ~ ., data=iris_train, write.forest = TRUE, 
                      mtry = 2,
                      num.trees = 500, 
                      replace = FALSE,
                      sample.fraction = .5,
                      min.node.size = 1)
  
  predict_brei <- predict(rf_brei, newdata = iris_test)
  predict_ranger <- predict(rf_ranger, dat = iris_test)$predictions
  
  MSE_brei[i] <- mean((iris_test$Sepal.Length - predict_brei)^2)
  MSE_ranger[i] <- mean((iris_test$Sepal.Length - predict_ranger)^2)
}


source("Rimp/hTree.R")
source("Rimp/hRandomForest.R")

MSE_my <- numeric()
for(i in 1:nsim){
  print(paste(i, " out of ", nsim))
  rf_my <- myRandomForest(iris_train[ , -1], iris_train$Sepal.Length, 
                          mtry = NULL, ntree = 500, nthreats = 1, 
                          minleafsize = 5, verbose = FALSE)
  
  predict_my <- predict(rf_my, iris_test[ , -1])
  MSE_my[i] <- mean((iris_test$Sepal.Length - predict_my)^2)
}

data.frame(kind = rep(c('Breiman', 'ranger' , 'my'), each = nsim),
           MSE = c(MSE_brei, MSE_ranger, MSE_my)) %>%
  ggplot() +
  geom_histogram(aes(x = MSE, fill = kind), bins = 100, position = 'identity', alpha = .5) +
  xlim(c(0.085, 0.12)) +
  ggtitle("Ranger and randomForest are not the same")



ggsave("Reports/figures/RangerMyRandomForest.pdf", width = 5, height = 2)

