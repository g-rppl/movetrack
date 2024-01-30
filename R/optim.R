#' Optimisation method for `track()`
#'
#' @description
#' Model flight path from point estimates using the optimisation method from
#'   `cmdstanr`.
#'
#' @param data A `data.frame` containing the point estimate data.
#' @param ... Additional arguments passed to `cmdstanr::optimize()`.
#'
#' @import dplyr
#' @importFrom cmdstanr cmdstan_model
#'
#' @export
#'
optim <- function(data, ...) {
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

    # Print progress
    cat(paste0("\nDone with ID ", i, ".\n \n"))
  }
  return(out)
}
