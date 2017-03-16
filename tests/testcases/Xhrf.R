library(hte)
# Example 1: Use Iris dataset
set.seed(1423614230)

feat <- iris[, -1]
tr <- rbinom(nrow(iris), 1, .5)
yobs <- iris[, 1]

xl <- X_RF(
  feat = feat,
  tr = tr,
  yobs = yobs,
  nthread = 1
)

EstimateCate(xl, feat)


# Example 2: large n
set.seed(400189)
n <- 10000
d <- 12

feat <- as.data.frame(matrix(rnorm(n*d), nrow = n))
tr <- rbinom(n, 1, feat$V1/ 20 +.5)
yobs <- feat$V1  + feat$V2 + tr * feat$V3

# this takes approximately 2 min to run.
xl <- X_RF(
  feat = feat,
  tr = tr,
  yobs = yobs
)


