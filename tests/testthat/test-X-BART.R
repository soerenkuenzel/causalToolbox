test_that("Tests that X-BART is working correctly",
          {
            # library(testthat)
            set.seed(1423614230)

            feat <- iris[, -1]
            tr <- rbinom(nrow(iris), 1, .5)
            yobs <- iris[, 1]

            xb <- X_BART(feat, tr, yobs, ensemble = "pscore", ndpost = 10)


            cate_estiamte <-
              EstimateCate(
                xb,
                feature_new = feat[5 - 9,],
                verbose = FALSE,
                return_CI = FALSE
              )
            expect_equal(cate_estiamte[1],
                         0.07646839,
                         tolerance = 1e-7)


            CIs <- CateCI(
              xb,
              feature_new = feat[5 - 9,],
              verbose = FALSE
            )
            expect_equal(as.numeric(CIs[2,]),
                         c(0.1496361, -0.1489134,  0.4486959),
                         tolerance = 1e-7)

            CIs <- CateCI(
              xb,
              feature_new = feat[5 - 9,],
              verbose = FALSE
            )
            expect_equal(as.numeric(CIs[2,]),
                         c(0.1645012, -0.1297062, 0.4618178),
                         tolerance = 1e-7)

            smpleStats <- EstimateAllSampleStatistics(xb)

            expect_equal(smpleStats$SATE[1,2], 0.001306513)
            expect_equal(smpleStats$SATT[1,2], -0.01310791)
            expect_equal(smpleStats$SATC[1,2], 0.01497213)
            expect_equal(smpleStats$CATE[1,2], 0.1022138)


            set.seed(112)
            xb <- X_BART(feat, tr, yobs,
                         ensemble = "pscore",
                         ndpost = 10,
                         tree_package = "BayesTree",
                         ntree = 200
                         )
            smpleStats <- EstimateAllSampleStatistics(xb)
            expect_equal(smpleStats$SATE[1,2], -0.003238378)

            set.seed(112)
            xb <- X_BART(feat, tr, yobs,
                         ensemble = "pscore",
                         ndpost = 10,
                         tree_package = "dbarts",
                         ntree = 100
            )
            smpleStats <- EstimateAllSampleStatistics(xb)
            expect_equal(smpleStats$SATE[1,2], 0.01730287)


          })
