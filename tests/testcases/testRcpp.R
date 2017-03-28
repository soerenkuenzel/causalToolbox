x <- iris[, -4]
y <- iris[, 4]
testForest <- honestRFRcpp(x, y, mtry=3, verbose=TRUE)
ypred <- predict(testForest, x)
sum((ypred - y)^2)


start_time <- Sys.time()
n <- 2500
p <- 10
x <- as.data.frame(matrix(rnorm(n*p), nrow = n))
y <- rnorm(n)
testForest <- honestRFRcpp(x, y, mtry=3, verbose=TRUE)
end_time <- Sys.time()

end_time - start_time
