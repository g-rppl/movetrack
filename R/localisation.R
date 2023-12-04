#' Estimate location
#'
#' @param data : \code{data.frame}. Motus data
#' @param det_range : \code{numeric}. Detection range of antennas in meters
#' @param dtime : \code{numeric}. Time interval in minutes
#'
#' @return \code{data.frame}
#'
#' @details
#'
#' - Estimate location for each detection: half of detection range
#'   along the directional beam
#' - Derive oscillating measurement error
#' - Weighted means (by signal strength) for each minute interval
#'
#' @import dplyr 
#' @import lubridate
#' @importFrom geosphere destPoint
#'
#' @export
#'
locate <- function(data = NULL, det_range = 12e3, dtime = 1) {
  data <- data %>% filter(!is.na(antBearing))

  # estimate location based on antenna bearing
  tmp <- as.data.frame(destPoint(
    data[, c("recvDeployLon", "recvDeployLat")],
    data$antBearing, det_range / 2
  ))

  # estimate oscillating error based on antenna bearing
  d <- cbind(data, tmp) %>%
    arrange(ts) %>%
    mutate(
      lon.sd = (det_range / 6e3) * sin(1 / 90 * pi * antBearing - pi / 2)
        + det_range / 3e3,
      lat.sd = (det_range / 6e3) * cos(1 / 90 * pi * antBearing)
        + det_range / 3e3
    )

  # transform to degrees
  d$lon.sd <- destPoint(d[, c("lon", "lat")], 90, d$lon.sd * 1000)[, 1] - d$lon
  d$lat.sd <- destPoint(d[, c("lon", "lat")], 0, d$lat.sd * 1000)[, 2] - d$lat

  # weighted means per minute interval
  d$ts.round <- round_date(d$ts, unit = paste(dtime, "min"))

  d <- d %>%
    group_by(ts.round) %>%
    mutate(
      lon.est = weighted.mean(lon, sig),
      lat.est = weighted.mean(lat, sig),
      lon.sd.est = weighted.mean(lon.sd, sig),
      lat.sd.est = weighted.mean(lat.sd, sig)
    ) %>%
    distinct(ts.round, .keep_all = TRUE)

  # proportions of time intervals
  d$w <- dtime / c(dtime, diff(as.numeric(d$ts.round) / 60))
  return(d)
}


#' Model data
#'
#' @param data : \code{data.frame}. Data
#' @param refresh : \code{numeric}. Refresh value for Stan
#'
#' @return \code{data.frame}
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
    file = "../Stan/DCRW.stan",
    data = stan.data,
    chains = 4,
    warmup = 1000,
    iter = 2000,
    cores = 4,
    refresh = refresh
  )
  return(fit)
}


#' Extract estimates
#'
#' @param fit : \code{stanfit}. Model output
#' @param mdata : \code{data.frame}. Metadata for each time interval
#' @param prob : \code{numeric}. Probability for HDI
#'
#' @return \code{data.frame}
#'
#' @details
#'
#' Returns mean, lower and upper bound of the highest density interval.
#'
#' @import HDInterval
#'
#' @export
#'
estimates <- function(fit, mdata = NULL, prob = 0.9) {
  HDI <- function(x) {
    hdi(c(x), credMass = prob)
  }

  drws <- as.matrix(fit, pars = "y")

  s <- data.frame(
    time = unique(mdata$ts.round),
    name = rep(c("lon", "lat"), each = length(mdata$ts)),
    mean = c(apply(drws, 2, mean)),
    lwr = c(apply(drws, 2, HDI)[1, ]),
    upr = c(apply(drws, 2, HDI)[2, ]),
    motusTagID = rep(mdata$motusTagID, 2),
    speciesEN = rep(mdata$speciesEN, 2)
  )
  return(s)
}


#' Calculate distances
#'
#' @param x : \code{data.frame}. Data in wide format
#'
#' @return \code{vector}
#'
#' @details
#'
#' Returns distances between consecutive locations in meters.
#'
#' @import geosphere
#'
#' @export
#'
distance <- function(x) {
  require(geosphere)
  dist <- rep(NA, nrow(x))
  for (i in 2:nrow(x)) {
    dist[i] <- distGeo(x[i, c("lon", "lat")], x[i-1, c("lon", "lat")])
  }
  return(dist)
}


#' Calculate speed
#'
#' @param x : \code{data.frame}. Data in wide format
#'
#' @return \code{vector}
#'
#' @details
#'
#' Returns speed between consecutive locations in m/s.
#'
#' @import lubridate
#'
#' @export
#'
speed <- function(x) {
  spd <- distance(x) / (c(NA, diff(x$time)) * 60)
  return(spd)
}
