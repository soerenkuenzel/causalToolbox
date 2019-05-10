
library(testthat)
test_that(
  "Tests that S-BART is working correctly",
  {
    context('S-BART')
    set.seed(1423614230)

    feat <- iris[, -1]
    tr <- rbinom(nrow(iris), 1, .5)
    yobs <- iris[, 1]

    set.seed(9822)
    sb <- S_BART(
      feat = feat,
      tr = tr,
      yobs = yobs,
      ndpost = 10)
    #theObject <- sb;verbose = TRUE
    EstimateCate(theObject = sb, feature_new = feat, verbose = FALSE)[1]

    expect_equal(EstimateCate(theObject = sb, feature_new = feat,
                              verbose = FALSE)[1],
                 0.1131117,
                 tolerance = 1e-7)

    set.seed(4533)

    CIs <- CateCI(sb, feat, verbose = FALSE)
    expect_equal(as.numeric(CIs[2, ]),
                 c(0.06648107, -0.00315152, 0.12503658),
                 tolerance = 1e-7)


    smpleStats <- EstimateAllSampleStatistics(sb)

    expect_equal(smpleStats$SATE[1,2], 0.01683829)
    expect_equal(smpleStats$SATT[1,2], 0.01072795)
    expect_equal(smpleStats$SATC[1,2], 0.02263122)
    expect_equal(smpleStats$CATE[1,2], 0.06011601)
})
