#' Model data
#'
#' @description
#' Model flight path from point estimates using a DCRW model.
#'
#' @param data A `data.frame` containing the point estimate data.
#' @param method The estimation method to use. Either `"mcmc"` or `"optim"`;
#'   defaults to `"mcmc"`.
#' @param ... Additional arguments passed to `cmdstanr::sample()` or
#'   `cmdstanr::optimize()`, respectively.
#'
#' @return
#' Returns a `stantrackr` object containing the posterior distributions for
#'   longitude, latitude, distance and speed.
#'
#' @details
#' This function calls [Stan](https://mc-stan.org/) via
#'   [cmdstanr](https://mc-stan.org/cmdstanr/index.html) and uses a difference
#'   correlated random walk model (DCRW) to estimate individual flight paths.
#'   The model is described in more detail in
#'   [Jonsen et al. 2005](https://doi.org/10.1890/04-1852) and
#'   [Baldwin et al. 2018](https://doi.org/10.1016/j.ecolmodel.2018.08.006). To
#'   learn more about state-space models in animal movement in general,
#'   [Auger-Méthé et al. 2021](https://doi.org/10.1002/ecm.1470) is a good
#'   starting point.
#'
#' @seealso `cmdstanr::sample()` `cmdstanr::optimize()`
#'
#' @importFrom dplyr %>% group_by summarise filter
#' @importFrom cmdstanr cmdstan_model
#'
#' @export
#'
track <- function(data, method = "mcmc", ...) {
  # Bind variables locally so that R CMD check doesn't complain
  . <- ID <- NULL

  # Check data
  if (is.null(data)) {
    stop("No data provided.")
  }

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

  # Compile model
  mod <- cmdstan_model(system.file("Stan", "DCRW.stan", package = "stantrackr"))

  for (i in unique(data$ID)) {
    # Subset data
    d <- data[data$ID == i, ]

    # Prepare data
    loc <- as.matrix(d[, c("lon", "lat")])
    sigma <- as.matrix(d[, c("lon_sd", "lat_sd")])

    # Bundle data
    stan.data <- list(
      loc = loc, sigma = sigma,
      N = nrow(loc), w = d$w
    )

    if (method == "mcmc") {
      # Sample
      fit <- mod$sample(data = stan.data, ...)

      # Summarise draws
      drws <- fit$draws("y")
      ids <- 1:(dim(drws)[3] / 2)
      lon <- drws[, , ids]
      lat <- drws[, , -c(ids)]
      distance <- .distanceMCMC(lon, lat)
      speed <- .speedMCMC(distance, d$ts)

      # Build output
      s <- list(list(
        ID = i,
        time = unique(d$ts),
        draws = list(
          lon = lon,
          lat = lat,
          distance = distance,
          speed = speed
        )
      ))

      # Add to output
      if (i == unique(data$ID)[1]) {
        out <- s
      } else {
        out <- append(out, s)
      }
      names(out) <- unique(data$ID)
      class(out) <- "stantrackr"
    } else if (method == "optim") {
      # Optimise
      fit <- mod$optimize(data = stan.data, init = list(list(y = loc)), ...)

      # Summarise result
      drws <- fit$summary("y")
      ids <- 1:(nrow(drws) / 2)
      lon <- drws$estimate[ids]
      lat <- drws$estimate[-c(ids)]
      distance <- .distance(lon, lat)
      speed <- .speed(distance, d$ts)

      # Build output
      s <- data.frame(
        ID = i,
        time = unique(d$ts),
        lon = lon,
        lat = lat,
        distance = distance,
        speed = speed
      )

      # Add to output
      if (i == unique(data$ID)[1]) {
        out <- s
      } else {
        out <- rbind(out, s)
      }
    } else {
      stop("Unknown method '", method, "'.")
    }

    # Print progress
    cat(paste0("\n Done with ID ", i, ".\n \n"))
  }
  return(out)
}
