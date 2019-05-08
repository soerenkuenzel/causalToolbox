library(testthat)
test_that("Tests that T-BART is working correctly", {
  context('T-BART')
  # library(testthat)
  set.seed(1423614230)
  
  feat <- iris[,-1]
  tr <- rbinom(nrow(iris), 1, .5)
  yobs <- iris[, 1]
  
  tb <- T_BART(
    feat,
    tr,
    yobs,
    ndpost = 10,
    nthread = 2,
    mu0.BART = list(sparse = FALSE, theta = 0, omega = 1, a = 0.5, b = 1, 
                    augment = FALSE, rho = NULL, usequants = FALSE, 
                    cont = FALSE, sigest = NA, sigdf = 3, sigquant = 0.9, k = 2,
                    power = 2, base = 0.95, sigmaf = NA, lambda = NA, 
                    ntree = 200L, numcut = 100L, ndpost = 1000L, nskip = 100L)
  )
  
  
  cate_estiamte <-
    EstimateCate(tb, feat[5 - 9,], verbose = FALSE)
  
  expect_equal(cate_estiamte[1], 0.0861047, tolerance = 1e-2)
  
  
  CIs <- CateCI(tb, feat, verbose = TRUE)
  
  expect_equal(as.numeric(CIs[2,]),
               c(-0.1820315, -0.5244863,  0.1174559),
               tolerance = 1e-7)
  
  smpleStats <- EstimateAllSampleStatistics(tb)
  
  expect_equal(smpleStats$SATE[1, 2], 0.1053239, tolerance = 1e-7)
  expect_equal(smpleStats$SATT[1, 2], 0.1350888, tolerance = 1e-7)
  expect_equal(smpleStats$SATC[1, 2], 0.08311957)
  expect_equal(smpleStats$CATE[1, 2], 0.08210535)
  
  set.seed(1111)
  tb <- T_BART(
    feat,
    tr,
    yobs,
    ndpost = 10,
    ntree = 200)
  smpleStats <- EstimateAllSampleStatistics(tb)
  
  expect_equal(smpleStats$SATE[1, 2], 0.1027337)
  
  set.seed(1111)
  tb <- T_BART(
    feat,
    tr,
    yobs,
    ndpost = 10,
    ntree = 100)
  smpleStats <- EstimateAllSampleStatistics(tb)
  expect_equal(smpleStats$SATT[1, 2], 0.1633545)
  
})
