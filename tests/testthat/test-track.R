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
  fit <- track(loc,
    parallel_chains = 4, seed = 42,
    refresh = 0, output_dir = NULL
  )

  expected <- c(
    319.0876, 319.1591, 314.4943, 323.6472, 2255.9851, 2256.0860,
    2252.7292, 2259.1937, 2106042, 593570.1623, 712.9220
  )
  expect_equal(colSums(fit[-c(1)], na.rm = TRUE), expected,
    ignore_attr = TRUE, tolerance = 0.01
  )
})
