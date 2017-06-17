library(testthat)
test_that("tests that RF is working", {
  set.seed(1242)
  x <- iris[,-1]
  y <- iris[, 1]
  # Preprocess the data
  preprocessedData <- preprocess_training(x, y)
  processed_x <- preprocessedData$x
  categoricalFeatureCols <- preprocessedData$categoricalFeatureCols
  categoricalFeatureMapping <-
    preprocessedData$categoricalFeatureMapping

  tree <- RFTree(
    x = processed_x,
    y = y,
    mtry = 2,
    nodesize = 2,
    # sampleIndex = sampleIndex,
    # splitrule = splitrule,
    categoricalFeatureCol = categoricalFeatureCols
  )

  feature.new = x
  # Preprocess the data
  processed_x <- preprocess_testing(feature.new,
                                    categoricalFeatureCols,
                                    categoricalFeatureMapping)
  expect_equal(
    predict(tree,
            processed_x,
            x,
            y,
            avgfunc = avgMean,
            categoricalFeatureCols)[1],
    5.266667,
    tolerance = 0.00001
  )
})
