devtools::load_all()
# Example 1: Use Iris dataset
set.seed(1423614230)

feat <- iris[, -c(1, 5)]
tr <- rbinom(nrow(iris), 1, .5)
yobs <- iris[, 1]

xl <- X_RF(
  feat = feat,
  tr = tr,
  yobs = yobs,
  nthread = 8
)

EstimateCate(xl, feat)
CateCI(xl, feat, B = 2)


