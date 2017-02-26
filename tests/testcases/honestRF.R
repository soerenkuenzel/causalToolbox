# Use Iris dataset
x <- iris[, -1]
x$Species <- as.numeric(x$Species)
y <- iris[, 1]

# Set seed for reproductivity
set.seed(24750371)

# Test honestRF (mimic RF)
forest <- honestRF(x, y, ntree=500, replace=TRUE,
                   sampsize=nrow(x),
                   mtry=3, nodesize=5,
                   nthread=10, splitrule="variance", avgfunc=avgMean,
                   splitratio=1, nodesizeAvg=5)

# Test predict
y_pred <- predict(forest, x)

# Mean Square Error
sum((y_pred - y)^2)
# 8.415478

# Test honestRF - half/half split
forest <- honestRF(x, y, ntree=1, replace=TRUE,
                   sampsize=nrow(x),
                   mtry=3, nodesize=3,
                   nthread=10, splitrule="variance", avgfunc=avgMean,
                   splitratio=0.5, nodesizeAvg=3)

# Test predict
y_pred <- predict(forest, x)

# Mean Square Error
sum((y_pred - y)^2)
# 17.75014

