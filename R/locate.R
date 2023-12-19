#' Estimate location
#'
#' @description
#' Calculate point estimates based on antenna bearing and signal strength.
#'
#' @param data A `data.frame` containing the telemetry data.
#' @param ID Unique identifier for individuals or tag deployments.
#' @param ts Timestamp column.
#' @param sig Signal strength column.
#' @param aLon Antenna longitude column.
#' @param aLat Antenna latitude column.
#' @param aBearing Antenna bearing column.
#' @param det_range Assumed maximum detection range of antennas in kilometres.
#' @param dtime Time interval in minutes for which point estimates are to be
#'   calculated.
#'
#' @return
#' Returns a `data.frame` containing estimated coordinates and measurement
#' errors for each time interval together with the proportions of time
#' intervals `w`.
#'
#' @details
#' This function performs the following steps as described in
#' [Baldwin et al. 2018](https://doi.org/10.1016/j.ecolmodel.2018.08.006):
#'
#'   - Estimate locations for each detection: half of the maximum detection
#'     range `det_range` along the directional beam.
#'   - Derive oscillating measurement error arising from antenna geometry and
#'     orientation.
#'   - Calculate weighted means (by signal strength) for each time interval
#'     `dtime`.
#'
#' @import dplyr
#' @import lubridate
#' @importFrom stats complete.cases weighted.mean
#'
#' @export
#'
locate <- function(
    data = NULL,
    ID = "tagDeployID",
    ts = "ts",
    sig = "sig",
    aLon = "recvDeployLon",
    aLat = "recvDeployLat",
    aBearing = "antBearing",
    det_range = 12,
    dtime = 2) {
  lon <- lat <- NULL

  # Build data
  d <- .buildData(data, ID, ts, sig, aLon, aLat, aBearing)

  # Estimate location based on antenna bearing
  tmp <- as.data.frame(.destPoint(
    d$aLon, d$aLat, d$aBearing, det_range / 2
  ))

  d <- cbind(d, tmp) %>% arrange(ID, ts)

  # Estimate oscillating error based on antenna bearing
  lon_sd <- (det_range / 6) * sin(1 / 90 * pi * d$aBearing - pi / 2) +
    det_range / 3
  lat_sd <- (det_range / 6) * cos(1 / 90 * pi * d$aBearing) +
    det_range / 3

  # Transform to degrees
  d$lon_sd <- .circDiff(.destPoint(d$lon, d$lat, 90, lon_sd)[, 1], d$lon)
  d$lat_sd <- abs(.destPoint(d$lon, d$lat, 0, lat_sd)[, 2] - d$lat)

  # Weighted means per minute interval
  d$ts <- round_date(d$ts, unit = paste(dtime, "min"))

  d <- d %>%
    group_by(ID, ts) %>%
    mutate(
      lon = weighted.mean(lon, sig),
      lat = weighted.mean(lat, sig),
      lon_sd = weighted.mean(lon_sd, sig),
      lat_sd = weighted.mean(lat_sd, sig)
    ) %>%
    distinct(ts, .keep_all = TRUE) %>%
    select(-c(sig, aLon, aLat, aBearing))

  # Proportions of time intervals
  d <- d %>%
    group_by(ID) %>%
    mutate(w = dtime / c(dtime, diff(as.numeric(ts) / 60)))

  return(as.data.frame(d))
}
