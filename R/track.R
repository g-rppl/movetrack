#' Model data
#'
#' @description
#' Model flight path from point estimates using a DCRW model.
#'
#' @param data A `data.frame` containing the point estimate data.
#' @param refresh The number of iterations between printed screen updates.
#'   If `refresh = 0`, only error messages will be printed.
#'
#' @return
#' A `CmdStanMCMC` object.
#'
#' @import cmdstanr
#'
#' @export
#'
track <- function(data = NULL, refresh = 100) {
  # Compile model
  mod <- cmdstan_model(
    file.path(system.file(package = "motusTrack"), "Stan", "DCRW.stan")
  )

  # Prepare data
  loc <- as.matrix(data[, c("lon", "lat")])
  sigma <- as.matrix(data[, c("lon_sd", "lat_sd")])

  # Bundle data
  stan.data <- list(
    loc = loc, sigma = sigma,
    N = nrow(loc), w = data$w
  )

  # Sample
  fit <- mod$sample(
    data = stan.data,
    chains = 4, parallel_chains = 4,
    refresh = refresh
  )
  return(fit)
}
