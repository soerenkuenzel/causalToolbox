
library(testthat)
test_that(
  "Tests that S-BART is working correctly",
  {
    context('S-BART')
    dir.create("knownTestValues/", showWarnings = FALSE)
    tmp <- "knownTestValues/testvalues-S_BART"
    
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
    # EstimateCate(theObject = sb, feature_new = feat, verbose = FALSE)[1]
    # set.seed(9822)
    # sb <- S_BART(
    #   feat = feat,
    #   tr = tr,
    #   yobs = yobs,
    #   ndpost = 10)
    # #theObject <- sb;verbose = TRUE
    # EstimateCate(theObject = sb, feature_new = feat, verbose = FALSE)[1]
    
    expect_silent(opt <- EstimateCate(theObject = sb, feature_new = feat)[1])
    expect_known_output(opt, file = tmp)
    set.seed(4533)

    expect_silent(CIs <- CateCI(sb, feat, verbose = FALSE))

    expect_silent(smpleStats <- EstimateAllSampleStatistics(sb))
    expect_known_output(smpleStats$SATE[1,2], file = tmp)
    expect_known_output(smpleStats$SATT[1,2], file = tmp)
    expect_known_output(smpleStats$SATC[1,2], file = tmp)
    expect_known_output(smpleStats$CATE[1,2], file = tmp)
})
