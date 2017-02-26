# Use Iris dataset
x <- iris[, -1]
x$Species <- as.numeric(x$Species)
y <- iris[, 1]

# Set seed for reproductivity
set.seed(24750371)

# Test RF
forest <- RF(x, y, ntree=500, replace=TRUE,
             sampsize=nrow(x),
             mtry=3, nodesize=5,
             nthread=10, splitrule="variance", avgfunc=avgMean)

# Test predict
y_pred <- predict(forest, x)

# Mean Square Error
sum((y_pred - y)^2)
# 8.616387
