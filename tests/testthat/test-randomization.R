library(testthat)

test_that("Tests that the randomization is working correctly", {
  #expect_equal(test_rnd(), 700, tolerance = 1e-8)

  expect_equal(test_rnd2(),
    c(9562, 729, 3776, 6260, 5082, 5527, 1788, 6911, 9257, 3550, 6689, 5217,
      3733, 8394, 1914, 1129, 2149, 1403, 1799, 5398, 8796, 6751, 6062, 2745,
      9980, 147, 2407, 5222, 2133, 4920, 906, 2663, 2449, 2769, 5022, 2381,
      4095, 9144, 7335, 287))
})
