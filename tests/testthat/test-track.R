data(motusData)
motusData$tagDeployID[1] <- 1
suppressMessages(loc <- locate(motusData, dtime = 10))

test_that("track", {
  expect_warning(fit <- track(loc,
    parallel_chains = 4, seed = 42,
    refresh = 0
  ))
  expected <- c(319.11, 2255.97, 674816.09, 836.96)
  expect_equal(colSums(as.data.frame(fit)[-c(1:2)], na.rm = TRUE), expected,
    ignore_attr = TRUE, tolerance = 0.01
  )

  expect_warning(fit <- track(loc,
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
