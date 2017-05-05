
library(testthat)
test_that(
  "Tests that S-BART is working correctly",
  {
    set.seed(1423614230)

    feat <- iris[, -1]
    tr <- rbinom(nrow(iris), 1, .5)
    yobs <- iris[, 1]

    sb <- S_BART(feat, tr, yobs, ndpost = 10, sample_stat =  "all estimated")
    #theObject <- sb;verbose = TRUE
    expect_equal(EstimateCate(sb, feat, verbose = FALSE)[1],
                 0.1082933,
                 tolerance = 1e-7)


    CIs <- CateCI(sb, feat, verbose = FALSE)
    expect_equal(as.numeric(CIs[2, ]),
                 c(0.16340923, 0.02277445, 0.30862431),
                 tolerance=1e-7)


    smpleStats <- EstimateAllSampleStatistics(sb)

    expect_equal(smpleStats$SATE[1,2], 0.01938017)
    expect_equal(smpleStats$SATT[1,2], 0.0200862)
    expect_equal(smpleStats$SATC[1,2], 0.01871081)
    expect_equal(smpleStats$CATE[1,2], 0.07840516)


})
