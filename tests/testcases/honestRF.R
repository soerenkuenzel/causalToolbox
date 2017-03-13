library(hte)

# Use Iris dataset
x <- iris[, -1]
y <- iris[, 1]

# Set seed for reproductivity
set.seed(24750371)

# Test honestRF (mimic RF)
forest <- honestRF(
  x,
  y,
  ntree=500,
  replace=TRUE,
  sampsize=nrow(x),
  mtry=3,
  nodesizeSpl=5,
  nthread=4,
  splitrule="variance",
  splitratio=1,
  nodesizeAvg=5
  )

# Test predict
y_pred <- predict(forest, x)

# Mean Square Error
sum((y_pred - y)^2)
# 8.460419

# Test honestRF - half/half split
forest <- honestRF(
  x,
  y,
  ntree=500,
  replace=TRUE,
  sampsize=nrow(x),
  mtry=3,
  nodesizeSpl=3,
  nthread=4,
  splitrule="variance",
  splitratio=0.5,
  nodesizeAvg=3
  )

# Test predict
y_pred <- predict(forest, x)

# Mean Square Error
sum((y_pred - y)^2)
# 11.76478

