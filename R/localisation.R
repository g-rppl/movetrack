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
est_loc <- function(data = NULL, det_range = 12e3, dtime = 1) {

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
#' @param refresh : \code{numeric}. refresh value for Stan
#'
#' @return \code{data.frame}
#'
#' @details
#'
#' returns location estimates for each time interval
#'
#' @import cmdstanr
#'
#' @export
#'
model_loc <- function(data = NULL, refresh = 100) {

  # compile model
  mod <- cmdstan_model("Stan/DCRW.stan")

  # prepare data
  loc <- as.matrix(data[, c("lon.est", "lat.est")])
  sigma <- as.matrix(data[, c("lon.sd.est", "lat.sd.est")])

  # bundle data
  stan.data <- list(
    loc = loc, sigma = sigma,
    N = nrow(loc), w = data$w
  )

  # sample
  fit <- mod$sample(
    data = stan.data,
    chains = 4, parallel_chains = 4,
    refresh = refresh
  )

  return(fit)
}


##### SUMMARISE POSTERIOR #####

summary_loc <- function(drws, mdata = NULL, prob = 0.9) {
  require(HDInterval)
  HDI <- function(x) {
    hdi(c(x), credMass = prob)
  }

  s <- data.frame(
    time = unique(mdata$ts),
    name = rep(c("lon", "lat"), each = length(mdata$ts)),
    mean = c(apply(drws, 3, mean)),
    lwr = c(apply(drws, 3, HDI)[1, ]),
    upr = c(apply(drws, 3, HDI)[2, ]),
    flightID = rep(mdata$flightID, 2),
    motudTagID = rep(mdata$motusTagID, 2),
    speciesEN = rep(mdata$speciesEN, 2)
  )

  return(s)
}