test_that("track", {
  expected <- c(319.11, 2255.97, 674816.09, 836.96)
  expect_equal(colSums(as.data.frame(fit)[-c(1:2)], na.rm = TRUE), expected,
    ignore_attr = TRUE, tolerance = 0.01
  )
})
