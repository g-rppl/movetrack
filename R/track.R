#' Model data
#'
#' @description
#' Model flight path from point estimates using a DCRW model.
#'
#' @param data A `data.frame` containing the point estimate data.
#' @param refresh The number of iterations between printed screen updates.
#'   If `refresh = 0`, only error messages will be printed.
#' @param prob The probability mass of the HDI.
#'
#' @return
#' Returns a list containing the model summary and the `CmdStanMCMC` object.
#'
#' @import cmdstanr
#' @importFrom HDInterval hdi
#'
#' @export
#'
track <- function(data = NULL, refresh = 100, prob = 0.9) {
  # Compile model
  mod <- cmdstan_model(system.file("Stan", "DCRW.stan", package = "motusTrack"))

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

  # HDI function
  HDI <- function(x) {
    hdi(c(x), credMass = prob)
  }

  # Summarise draws
  drws <- fit$draws("y")

  s <- data.frame(
    time = unique(data$ts),
    name = rep(c("lon", "lat"), each = length(data$ts)),
    mean = c(apply(drws, 3, mean)),
    lwr = c(apply(drws, 3, HDI)[1, ]),
    upr = c(apply(drws, 3, HDI)[2, ])
  )
  return(list(summary = s, stanfit = fit))
}
