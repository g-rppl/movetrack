#' Model data
#' 
#' @description 
#' Model data using a DCRW model.
#'
#' @param data `data.frame` Data
#' @param refresh `numeric` Refresh value for Stan
#'
#' @return a `data.frame`
#'
#' @details
#'
#' Returns location estimates for each time interval.
#'
#' @import rstan
#'
#' @export
#'
track <- function(data = NULL, refresh = 100) {

  # prepare data
  loc <- as.matrix(data[, c("lon.est", "lat.est")])
  sigma <- as.matrix(data[, c("lon.sd.est", "lat.sd.est")])

  # bundle data
  stan.data <- list(
    loc = loc, sigma = sigma,
    N = nrow(loc), w = data$w
  )

  # sample
  fit <- stan(
    file = file.path(system.file(package = "motusTrack"), "Stan", "DCRW.stan"),
    data = stan.data,
    chains = 4,
    warmup = 1000,
    iter = 2000,
    cores = 4,
    refresh = refresh
  )
  return(fit)
}