test_that("track", {
  expect_error(track(loc, method = "foo"),
  "Unknown method 'foo'")

  expected <- c(319.11, 2255.97, 674816.09, 836.96)
  expect_equal(colSums(as.data.frame(fit)[-c(1:2)], na.rm = TRUE), expected,
    ignore_attr = TRUE, tolerance = 0.01
  )

  expect_warning(fit <- track(loc2,
    method = "optim",
    seed = 42,
    refresh = 0
  ))
  expected <- c(319.39, 2256.60, 579519.35, 739.24)
  expect_equal(
    colSums(fit[, c("lon", "lat", "distance", "speed")], na.rm = TRUE),
    expected,
    ignore_attr = TRUE, tolerance = 0.01
  )
})
