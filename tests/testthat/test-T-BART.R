library(testthat)
test_that("Tests that T-BART is working correctly",
          {
            # library(testthat)
            set.seed(1423614230)

            feat <- iris[, -1]
            tr <- rbinom(nrow(iris), 1, .5)
            yobs <- iris[, 1]

            tb <- T_BART(feat, tr, yobs, ndpost = 10,
                         sample_stat =  "all estimated")


            cate_estiamte <-
              EstimateCate(tb, feat[5 - 9, ], verbose = FALSE)

            expect_equal(cate_estiamte[1],
                         0.04991787,
                         tolerance = 1e-7)


            CIs <- CateCI(tb, feat, verbose = FALSE)

            expect_equal(as.numeric(CIs[2, ]),
                         c(0.31184745, -0.01854036, 0.57969852),
                         tolerance = 1e-7)

            smpleStats <- EstimateAllSampleStatistics(tb)

            expect_equal(smpleStats$SATE[1,2], 0.005437859)
            expect_equal(smpleStats$SATT[1,2], -0.02539383)
            expect_equal(smpleStats$SATC[1,2], 0.0346679)
            expect_equal(smpleStats$CATE[1,2], -0.01047405)

          })
