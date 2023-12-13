#' Model data
#'
#' @description
#' Model flight path from point estimates using a DCRW model.
#'
#' @param data A `data.frame` containing the point estimate data.
#' @param ci The method used to calculate the credible intervals. Available
#'   options are `'HDI'` for the highest posterior density interval and `'ETI'`
#'   for the equal-tailed interval; defaults to `'HDI'`.
#' @param prob The probability mass of the credible interval; defaults to `0.9`.
#' @param ... Additional arguments passed to `cmdstanr::sample()`.
#'
#' @return
#' Returns a list containing the model summary and the `CmdStanMCMC` object.
#'
#' @import lubridate
#' @import cmdstanr
#' @importFrom stats median quantile
#' @importFrom HDInterval hdi
#'
#' @export
#'
track <- function(data = NULL, ci = "HDI", prob = 0.9, ...) {
  # Bind variables locally so that R CMD check doesn't complain
  name <- NULL

  # Check data
  if (is.null(data)) {
    stop("No data provided.")
  }
  if (!ci %in% c("HDI", "ETI")) {
    stop(
      "Invalid credible interval method. Available options are 'HDI' and 'ETI'."
    )
  }
  if (prob < 0 || prob > 1) {
    stop("Probability mass must be between 0 und 1.")
  }

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
  fit <- mod$sample(data = stan.data, ...)

  # Summarise draws
  drws <- fit$draws("y")

  s <- data.frame(
    time = unique(data$ts),
    name = rep(c("lon", "lat"), each = length(data$ts)),
    mean = c(apply(drws, 3, mean)),
    median = c(apply(drws, 3, median)),
    lwr = if (ci == "ETI") {
      c(apply(drws, 3, quantile, prob = (1 - prob) / 2))
    } else {
      c(apply(drws, 3, hdi, credMass = prob)[1, ])
    },
    upr = if (ci == "ETI") {
      c(apply(drws, 3, quantile, prob = 1 - (1 - prob) / 2))
    } else {
      c(apply(drws, 3, hdi, credMass = prob)[2, ])
    }
  ) %>%
    group_split(name, .keep = FALSE)

  # Wide format and additional variables
  s <- merge(s[[2]], s[[1]], by = "time", suffix = c(".lon", ".lat"))
  s$distance <- .distance(s)
  s$speed <- s$distance / (c(NA, diff(s$time)) * 60)

  return(list(summary = s, stanfit = fit))
}
