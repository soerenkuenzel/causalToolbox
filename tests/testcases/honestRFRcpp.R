library(hte)
library(pryr)
library(ranger)
library(randomForest)
mem_used()

x <- iris[, -4]
y <- iris[, 4]
testForest <- honestRFRcpp(x, y, mtry=3, verbose=TRUE)
ypred <- predict(testForest, x)
sum((ypred - y)^2)


start_time <- Sys.time()
n <- 10000
p <- 10
x <- as.data.frame(matrix(rnorm(n*p), nrow = n))
y <- rnorm(n)


test = rcpp_cppBuildInterface(x, y, vector(), n, p, 100, TRUE, n, 3, 1, 5, 5, 24750371, FALSE)
rm(test)
gc(verbose = TRUE)

testForest <- ranger(y ~. ,data=data.frame(x,y))
testForest <- honestRFRcpp(x, y)
rm(testForest)
gc(verbose = TRUE)

testForest <- randomForest(x, y)
end_time <- Sys.time()
end_time - start_time
