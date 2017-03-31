library(hte)

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
testForest <- randomForest(x, y, verbose=FALSE)
end_time <- Sys.time()
end_time - start_time
