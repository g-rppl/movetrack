#' Model data
#'
#' @description
#' Model flight path from point estimates using a DCRW model.
#'
#' @param data A `data.frame` containing the point estimate data.
#' @param states The number of states to use in the model; defaults to `1`.
#' @param i_lambda Logical indicating whether to estimate individual correlation
#'   parameters; defaults to `TRUE`.
#' @param vars A `matrix` or `data.frame` containing covariates to include in
#'   the state process model; should have the same number of rows as `data`.
#' @param method The estimation method to use. Either `"mcmc"` or `"optim"`;
#'   defaults to `"mcmc"`.
#' @param ... Additional arguments passed to `cmdstanr::sample()` or
#'   `cmdstanr::optimize()`, respectively.
#'
#' @details
#' This function calls [Stan](https://mc-stan.org/) via
#' [cmdstanr](https://mc-stan.org/cmdstanr/index.html) and uses a difference
#' correlated random walk model (DCRW) to estimate individual flight paths.
#' The model is described in more detail in
#' [Jonsen et al. 2005](https://doi.org/10.1890/04-1852) and
#' [Baldwin et al. 2018](https://doi.org/10.1016/j.ecolmodel.2018.08.006). To
#' learn more about state-space models in animal movement in general,
#' [Auger-Méthé et al. 2021](https://doi.org/10.1002/ecm.1470) is a good
#' starting point. Available estimation methods are Stan's main Markov chain
#' Monte Carlo algorithm and Stan's optimizer to obtain a (penalized) maximum
#' likelihood estimate or a maximum a posteriori estimate (if `jacobian=TRUE`).
#' See the [CmdStan User's Guide](https://mc-stan.org/docs/cmdstan-guide) for
#' more details.
#'
#' @return
#' Returns a `data.frame` containing estimates for longitude, latitude, distance
#' and speed per time interval or a `stantrackr` object including the full
#' posterior distributions.
#'
#' @seealso `cmdstanr::sample()` `cmdstanr::optimize()`
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
#' track(loc, "optim", refresh = 1e3)
#' }
#'
#' @importFrom dplyr %>% n group_by summarise filter
#' @importFrom cmdstanr cmdstan_model
#'
#' @export
#'
track <- function(
    data,
    states = 1, i_lambda = TRUE, vars = NULL, method = "mcmc", ...) {
  # Bind variables locally so that R CMD check doesn't complain
  . <- ID <- NULL

  # Check data
  ids <- data %>%
    group_by(ID) %>%
    summarise(n = n()) %>%
    filter(n < 3) %>%
    .$ID
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
  covs <- as.matrix(cbind(matrix(1, nrow = nrow(y)), vars))

  # Bundle data
  stan.data <- list(
    T = nrow(d), I = length(unique(d$ID)), N = states, nCovs = ncol(covs) - 1,
    y = y, sigma = sigma, w = d$w,
    index = index,
    covs = covs,
    i_lambda = as.numeric(i_lambda)
  )

  if (method == "mcmc") {
    # Sample
    fit <- mod$sample(data = stan.data, ...)

    # Summarise draws
    drws <- fit$draws("z")
    idx <- 1:(dim(drws)[3] / 2)
    lon <- drws[, , idx]
    lat <- drws[, , -c(idx)]
    distance <- .distanceMCMC(lon, lat, index)
    speed <- .speedMCMC(distance, d$ts)

    # Build output
    out <- list(
      ID = d$ID,
      time = d$ts,
      draws = list(
        lon = lon,
        lat = lat,
        distance = distance,
        speed = speed
      ),
      beta = fit$draws("beta")
    )
    class(out) <- "stantrackr"
  } else if (method == "optim") {
    # Optimise
    fit <- mod$optimize(data = stan.data, init = list(list(y = y)), ...)

    # Summarise result
    drws <- fit$summary("z")
    idx <- 1:(nrow(drws) / 2)
    lon <- drws$estimate[idx]
    lat <- drws$estimate[-c(idx)]
    distance <- .distance(lon, lat, index)
    speed <- .speed(distance, d$ts)

    # Build output
    out <- data.frame(
      ID = d$ID,
      time = d$ts,
      lon = lon,
      lat = lat,
      distance = distance,
      speed = speed
    )
  } else {
    stop("Unknown method '", method, "'.")
  }
  return(out)
}
