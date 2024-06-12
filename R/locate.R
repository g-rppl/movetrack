#' Estimate locations
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
#' @param aType Antenna type column, only required for antenna-specific
#'   detection ranges.
#' @param aBearing Antenna bearing column.
#' @param aRange Assumed maximum detection range of antennas in kilometres.
#'   Can be a single value or a named list of values for different antenna
#'   types.
#' @param dTime Time interval in minutes for which point estimates are to be
#'   calculated.
#'
#' @details
#' This function performs the following steps as described in
#' [Baldwin et al. 2018](https://doi.org/10.1016/j.ecolmodel.2018.08.006):
#'
#'   - Estimate locations for each detection: half of the maximum detection
#'     range `aRange` along the directional beam.
#'   - Derive oscillating measurement error arising from antenna geometry and
#'     orientation.
#'   - Calculate weighted means (by signal strength) for each time interval
#'     `dTime`.
#'
#' @return
#' Returns a `data.frame` containing estimated coordinates and measurement
#' errors for each time interval together with the proportions of time
#' intervals `w`.
#'
#' @examples
#' \dontrun{
#' data(motusData)
#' locate(motusData)
#' locate(motusData, dTime = 1, aRange = 10)
#' locate(motusData, aType = "antType", aRange = list("yagi-5"=10, "yagi-6"=12))
#' }
#'
#' @importFrom dplyr arrange distinct group_by mutate select any_of
#' @importFrom lubridate round_date is.POSIXct
#' @importFrom stats complete.cases weighted.mean
#' @importFrom ggplot2 .data
#'
#' @export
#'
locate <- function(
    data,
    ID = "tagDeployID",
    ts = "ts",
    sig = "sig",
    aLon = "recvDeployLon",
    aLat = "recvDeployLat",
    aType = NULL,
    aBearing = "antBearing",
    aRange = 12,
    dTime = 2) {
  # Build data
  d <- .buildData(data, ID, ts, sig, aLon, aLat, aType, aBearing, aRange)

  # Estimate location based on antenna bearing
  tmp <- as.data.frame(.destPoint(
    d$aLon, d$aLat, d$aBearing, d$aRange / 2
  ))

  d <- cbind(d, tmp) |> arrange(ID, ts)

  # Estimate oscillating error based on antenna bearing
  lon_sd <- (d$aRange / 6) * sin(1 / 90 * pi * d$aBearing - pi / 2) +
    d$aRange / 3
  lat_sd <- (d$aRange / 6) * cos(1 / 90 * pi * d$aBearing) +
    d$aRange / 3

  # Transform to degrees
  d$lon_sd <- .circDiff(.destPoint(d$lon, d$lat, 90, lon_sd)[, 1], d$lon)
  d$lat_sd <- abs(.destPoint(d$lon, d$lat, 0, lat_sd)[, 2] - d$lat)

  # Weighted means per minute interval
  d$ts <- round_date(d$ts, unit = paste(dTime, "min"))

  d <- d |>
    # rescale signal strengths, but ensure that min(sig) > 0
    mutate(sig = sig - min(sig) + 1e-10) |>
    group_by(ID, ts) |>
    mutate(
      lon = weighted.mean(.data$lon, sig),
      lat = weighted.mean(.data$lat, sig),
      lon_sd = weighted.mean(lon_sd, sig),
      lat_sd = weighted.mean(lat_sd, sig)
    ) |>
    distinct(ts, .keep_all = TRUE) |>
    select(-c(sig, aLon, aLat, aBearing, aRange)) |>
    select(-any_of("aType"))

  # Proportions of time intervals
  d <- d |>
    group_by(ID) |>
    mutate(w = dTime / c(dTime, diff(as.numeric(ts) / 60)))

  return(as.data.frame(d))
}
