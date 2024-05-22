test_that("locate error", {
  expect_error(
    locate(motusData, det_range = "loo"),
    "'det_range' must be numeric or a named list."
  )
  expect_error(
    locate(motusData, det_range = list("yagi-6" = 12)),
    "Missing detection range for 'yagi-5'."
  )
})

test_that("locate result", {
  expect_message(
    loc <- locate(motusData, det_range = list("yagi-5" = 10, "yagi-6" = 12)),
    "Removed 10 detections containing missing values."
  )
  expect_true(is.data.frame(loc))
  expect_equal(nrow(loc), 161)
  expect_equal(ncol(loc), 7)
  expect_true(is.POSIXct(loc$ts))

  expected <- c(
    1224.17860790, 8657.90549885, 9.18330504, 6.07470543, 149.358945
  )
  expect_equal(colSums(loc[-c(1:2)]), expected, ignore_attr = TRUE)
})
