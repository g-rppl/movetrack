data(motusData)

test_that("locate", {
  expect_message(
    loc <- locate(motusData, dtime = 2),
    "Removed 10 detections containing missing values."
  )
  expect_true(is.data.frame(loc))
  expect_equal(nrow(loc), 161)
  expect_equal(ncol(loc), 7)
  expect_true(is.POSIXct(loc$ts))

  expected <- c(1224.1888, 8657.874, 9.2656, 6.0957, 149.3589)
  expect_equal(colSums(loc[-c(1:2)]), expected, ignore_attr = TRUE)
})
