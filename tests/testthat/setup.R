data(motusData)
suppressMessages(loc <- locate(motusData, dTime = 10))
suppressMessages(loc$ID[1] <- 1)

check_cmdstan_toolchain(fix = TRUE)
install_cmdstan(cores = 4, check_toolchain = FALSE)
set_cmdstan_path()

test_that("track warnings", {
  expect_warning(
    fit <<- track(loc, seed = 42, parallel_chains = 4, refresh = 0)
  )
})
