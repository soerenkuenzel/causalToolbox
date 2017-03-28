x <- iris[, -4]
y <- iris[, 4]
testForest <- honestRFRcpp(x, y, mtry=3)
ypred <- predict(testForest, x)
sum((ypred - y)^2)
