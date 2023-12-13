test_that("track errors", {
  expect_error(track(), "No data provided.")
  expect_error(
    track(testdata, ci = "foo"),
    "Invalid credible interval method. Available options are 'HDI' and 'ETI'."
  )
  expect_error(
    track(testdata, prob = 2),
    "Probability mass must be between 0 und 1."
  )
})
