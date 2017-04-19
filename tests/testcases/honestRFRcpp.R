library(hte)
library(pryr)
library(ranger)
library(randomForest)
mem_used()

# Iris example
x <- iris[, -4]
y <- iris[, 4]
testForest <- honestRF(x, y, mtry=3, verbose=TRUE)
ypred <- predict(testForest, x)
sum((ypred - y)^2)


# Random Numbers
n <- 10000
p <- 10
x <- as.data.frame(matrix(rnorm(n*p), nrow = n))
y <- rnorm(n)

# Time 11 seconds
# Create Memory + 250 MB
# Remove Memory - 118 MB
start_time <- Sys.time()
testForest <- ranger(y ~. ,data=data.frame(x,y))
end_time <- Sys.time()
end_time - start_time
rm(testForest)
gc(verbose = TRUE)

# Time 23 seconds
# Create Memory + 352 MB
# Remove Memory - 290 MB
start_time <- Sys.time()
testForest <- honestRF(x, y)
y_pred <- predict(testForest, x)
end_time <- Sys.time()
end_time - start_time
rm(testForest)
gc(verbose = TRUE)

# Time 59 seconds
# Create Memory + 548 MB
# Remove Memory - 0 MB
start_time <- Sys.time()
testForest <- randomForest(x, y)
end_time <- Sys.time()
end_time - start_time
rm(testForest)
gc(verbose = TRUE)
