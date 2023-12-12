data(testdata)

test_that("locate", {
  loc <- locate(testdata, dtime = 2)
  expect_true(is.data.frame(loc))
  expect_equal(nrow(loc), 93)
  expect_equal(ncol(loc), 6)
  expect_true(is.POSIXct(loc$ts))

  expected <- c(632.980213, 4970.015813, 5.027254, 3.685180, 86.122254)
  expect_equal(colSums(loc[-c(1)]), expected, ignore_attr = TRUE)
})

test_that("locate message", {
  testdata$sig[1] <- NA
  expect_message(
    locate(testdata, dtime = 2),
    "Removed 1 detections containing missing values."
  )
})
