devtools::load_all()
library(hte)
# Use Iris dataset
set.seed(1423614230)

feat <- iris[, -1]
tr <- rbinom(nrow(iris), 1, .5)
yobs <- iris[, 1]

xl <- X_RF(
  feat = feat,
  tr = tr,
  yobs = yobs
)

EstimateCate(xl, feat)
