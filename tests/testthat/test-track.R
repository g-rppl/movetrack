test_that("track", {
  expected <- c(310.1, 2201.3, 649681.3, 799.5)
  expect_equal(colSums(as.data.frame(fit)[-c(1:2)], na.rm = TRUE), expected,
    ignore_attr = TRUE, tolerance = 0.01
  )
})
