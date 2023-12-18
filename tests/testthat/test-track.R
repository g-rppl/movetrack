data(motusData)
motusData$tagDeployID[1] <- 1
suppressMessages(loc <- locate(motusData, dtime = 10))

test_that("track errors", {
  expect_error(track(), "No data provided.")
  expect_error(
    track(loc, ci = "foo"),
    "Invalid credible interval method. Available options are 'HDI' and 'ETI'."
  )
  expect_error(
    track(loc, prob = 2),
    "Probability mass must be between 0 und 1."
  )
})

test_that("track", {
  expect_warning(fit <- track(loc,
    parallel_chains = 4, seed = 42,
    refresh = 0, output_dir = NULL
  ))
  expected <- c(
    319.09, 319.16, 314.49, 323.65, 2255.99, 2256.09,
    2252.73, 2259.19, 2106042, 593570.16, 712.92
  )
  expect_equal(colSums(fit[-c(1)], na.rm = TRUE), expected,
    ignore_attr = TRUE, tolerance = 0.01
  )

  expect_warning(fit <- track(loc,
    parallel_chains = 4, seed = 42,
    refresh = 0, output_dir = NULL,
    ci = "ETI"
  ))
  expected <- c(314.20, 323.79, 2252.45, 2259.12)
  expect_equal(
    colSums(fit[, c("lwr.lon", "upr.lon", "lwr.lat", "upr.lat")], na.rm = TRUE),
    expected,
    ignore_attr = TRUE, tolerance = 0.01
  )
})
