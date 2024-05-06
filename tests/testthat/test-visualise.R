test_that("plot", {
  expect_error(plot(fit), NA)
})

test_that("map", {
  expect_true("ggplot" %in% class(mapTrack(fit)))
})
