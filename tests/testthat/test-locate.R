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
    1223.96643673, 8657.94836071, 9.08645214, 6.11561118, 149.358945
  )
  expect_equal(colSums(loc[-c(1:2)]), expected, ignore_attr = TRUE)
})
