test_that("track", {
  expected <- c(310, 2201, 665463, 823)
  expect_equal(colSums(as.data.frame(fit)[-c(1:2)], na.rm = TRUE), expected,
    ignore_attr = TRUE, tolerance = 0.01
  )
})
