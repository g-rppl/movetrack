#' Model data
#'
#' @description
#' Model flight path from point estimates using a Hidden Markov Model.
#'
#' @param data A `data.frame` containing the point estimate data.
#' @param states The number of states to use in the model; defaults to `1`.
#' @param i_lambda Logical indicating whether to estimate individual correlation
#'   parameters; defaults to `TRUE`.
#' @param ... Additional arguments passed to `cmdstanr::sample()`.
#'
#' @details
#' This function calls [Stan](https://mc-stan.org/) via
#' [cmdstanr](https://mc-stan.org/cmdstanr/index.html) and uses a Hidden Markov
#' Model (HMM) to estimate individual flight paths.
#'
#' @return
#' Returns a `stantrackr` object including the posterior distributions for
#' longitude, latitude, distance, and speed per time interval.
#'
#' @references
#' Auger‐Méthé, M., Newman, K., Cole, D., Empacher, F., Gryba, R., King, A. A.,
#' ... & Thomas, L. (2021). A guide to state–space modeling of ecological time
#' series. *Ecological Monographs*, 91(4), e01470.
#' doi:[10.1002/ecm.1470](https://doi.org/10.1002/ecm.1470)
#'
#' Baldwin, J. W., Leap, K., Finn, J. T., & Smetzer, J. R. (2018). Bayesian
#' state-space models reveal unobserved off-shore nocturnal migration from Motus
#'  data. *Ecological Modelling*, 386, 38-46.
#' doi:[10.1016/j.ecolmodel.2018.08.006](
#' https://doi.org/10.1016/j.ecolmodel.2018.08.006)
#'
#' Jonsen, I. D., Flemming, J. M., & Myers, R. A. (2005). Robust state–space
#' modeling of animal movement data. *Ecology*, 86(11), 2874-2880.
#' doi:[10.1890/04-1852](https://doi.org/10.1890/04-1852)
#'
#' @seealso `cmdstanr::sample()`
#'
#' @examples
#' \dontrun{
#' # Load data
#' data(motusData)
#'
#' # Estimate locations
#' loc <- locate(motusData, dtime = 2)
#'
#' # Model flight paths
#' track(loc, states = 2, parallel_chains = 4)
#' track(loc, i_lambda = FALSE, parallel_chains = 4)
#' }
#'
#' @importFrom dplyr n group_by summarise filter
#' @importFrom ggplot2 .data
#' @importFrom cmdstanr cmdstan_model
#'
#' @export
#'
track <- function(data, states = 1, i_lambda = TRUE, ...) {
  # Check data
  ids <- data |>
    group_by(.data$ID) |>
    summarise(n = n()) |>
    filter(n < 3) |>
    _$ID
  if (length(ids) > 0) {
    warning(paste0(
      ifelse(length(ids) < 2, "ID ", "IDs "), paste(ids, collapse = ", "),
      " had less than 3 observations and ",
      ifelse(length(ids) < 2, "was", "were"),
      " not modelled. You may want to decrease the `dtime` argument in",
      " `locate()` to increase the number of observations."
    ))
    data <- data[!data$ID %in% ids, ]
  }
  d <- data

  # Compile model
  mod <- cmdstan_model(system.file("Stan", "HMM.stan", package = "stantrackr"))

  # Prepare data
  y <- as.matrix(d[, c("lon", "lat")])
  sigma <- as.matrix(d[, c("lon_sd", "lat_sd")])
  index <- c(0, which(diff(as.numeric(as.factor(d$ID))) != 0), nrow(d))

  # Bundle data
  stan.data <- list(
    T = nrow(d), I = length(unique(d$ID)), N = states,
    y = y, sigma = sigma, w = d$w,
    index = index,
    i_lambda = as.numeric(i_lambda)
  )

  # Sample
  fit <- mod$sample(data = stan.data, ...)

  # Summarise draws
  drws <- fit$draws("z")
  idx <- 1:(dim(drws)[3] / 2)
  lon <- drws[, , idx]
  lat <- drws[, , -c(idx)]
  distance <- .distance(lon, lat, index)
  speed <- .speed(distance, d$ts)

  # Build output
  out <- list(
    ID = d$ID,
    time = d$ts,
    draws = list(
      lon = lon,
      lat = lat,
      distance = distance,
      speed = speed
    )
  )
  class(out) <- "stantrackr"
  return(out)
}
