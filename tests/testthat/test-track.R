test_that("track", {
  expected <- c(310, 2201, 696493, 869)
  expect_equal(colSums(as.data.frame(fit)[-c(1:2)], na.rm = TRUE), expected,
    ignore_attr = TRUE, tolerance = 0.01
  )
})
