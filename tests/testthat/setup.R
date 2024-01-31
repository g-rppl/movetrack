data(motusData)
suppressMessages(loc <- locate(motusData, dtime = 10))
suppressMessages(fit <- track(loc, parallel_chains = 4, refresh = 0))
