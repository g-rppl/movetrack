data(motusData)
suppressMessages(loc <- locate(motusData, dtime = 10))
suppressMessages(loc$ID[1] <- 1)

test_that("track warnings", {
  expect_warning(
    fit <<- track(loc, seed = 42, parallel_chains = 4, refresh = 0)
  )
})
