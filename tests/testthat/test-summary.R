test_that("summary errors", {
  expect_error(
    summary(fit, var = "foo"),
    paste(
      "Invalid variable. Available options are 'lon', 'lat', 'distance', and",
      "'speed'."
    )
  )
  expect_error(
    summary(fit, ci = "foo"),
    "Invalid credible interval method. Available options are 'HDI' and 'ETI'."
  )
  expect_error(
    summary(fit, prob = 2),
    "Probability mass must be between 0 und 1."
  )
})

test_that("summary types", {
  expect_true(is.data.frame(as.data.frame(fit)))
  expect_true(is.data.frame(print(fit)))
})

test_that("summary result", {
  expect_equal(
    colSums(summary(fit)[-c(1:2)]),
    c(319.08, 319.15, 314.53, 323.64, 2255.98, 2256.07, 2252.69, 2259.25),
    ignore_attr = TRUE, tolerance = 0.01
  )
  expect_equal(
    colSums(summary(fit, var = "distance", ci = "ETI")[-c(1:2)], na.rm = TRUE),
    c(677802.0, 658509.7, 327016.2, 1099289.4),
    ignore_attr = TRUE, tolerance = 0.01
  )
})
