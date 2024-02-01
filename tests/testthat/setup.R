data(motusData)
suppressMessages(loc <- loc2 <- locate(motusData, dtime = 10))
suppressMessages(loc2$ID[1] <- 1)
suppressMessages(fit <- track(loc, seed = 42, parallel_chains = 4, refresh = 0))
