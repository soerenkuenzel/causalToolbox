
library(testthat)
test_that(
  "Tests that S-BART is working correctly",
  {
    set.seed(1423614230)

    feat <- iris[, -1]
    tr <- rbinom(nrow(iris), 1, .5)
    yobs <- iris[, 1]

    set.seed(9822)
    sb <- S_BART(feat, tr, yobs, ndpost = 10,
                 sample_stat =  "all estimated",
                 tree_package = "BayesTree")
    #theObject <- sb;verbose = TRUE
    expect_equal(EstimateCate(sb, feat, verbose = FALSE)[1],
                 0.08339891,
                 tolerance = 1e-7)
    set.seed(9822)
    sb <- S_BART(feat, tr, yobs, ndpost = 10,
                 sample_stat =  "all estimated",
                 tree_package = "dbarts")
    #theObject <- sb;verbose = TRUE
    expect_equal(EstimateCate(sb, feat, verbose = FALSE)[1],
                 0.06038749,
                 tolerance = 1e-7)

    set.seed(4533)

    CIs <- CateCI(sb, feat, verbose = FALSE)
    expect_equal(as.numeric(CIs[2, ]),
                 c(0.094355273, 0.004000758, 0.233720020),
                 tolerance=1e-7)


    smpleStats <- EstimateAllSampleStatistics(sb)

    expect_equal(smpleStats$SATE[1,2], 0.01683829)
    expect_equal(smpleStats$SATT[1,2], 0.01072795)
    expect_equal(smpleStats$SATC[1,2], 0.02263122)
    expect_equal(smpleStats$CATE[1,2], 0.06011601)


    set.seed(9822)
    sb <- S_BART(feat, tr, yobs, ndpost = 10,
                 sample_stat =  "all estimated",
                 tree_package = "dbarts",
                 ntree = 500)
    #theObject <- sb;verbose = TRUE
    expect_equal(EstimateCate(sb, feat, verbose = FALSE)[1],
                 0.04363598,
                 tolerance = 1e-7)

})
