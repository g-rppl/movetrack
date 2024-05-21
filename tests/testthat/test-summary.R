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
    c(310, 310, 306, 315, 2201, 2201, 2198, 2204),
    ignore_attr = TRUE, tolerance = 0.01
  )
  expect_equal(
    colSums(summary(fit, var = "distance", ci = "ETI")[-c(1:2)], na.rm = TRUE),
    c(696493, 676289, 327763, 1137128),
    ignore_attr = TRUE, tolerance = 0.01
  )
})

test_that("getDraws", {
  drws <- getDraws(fit)
  expect_true(is.data.frame(drws))
  expect_equal(dim(drws), c(2050, 7))
})
