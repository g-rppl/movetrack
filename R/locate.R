#' Estimate location
#'
#' @description
#' Estimate location based on antenna bearing and signal strength.
#'
#' @param data `data.frame` Motus data
#' @param det_range `numeric` Detection range of antennas in kilometers
#' @param dtime `numeric` Time interval in minutes
#'
#' @return a `data.frame`
#'
#' @details
#'
#'   - Estimate location for each detection: half of detection range
#'     along the directional beam
#'   - Derive oscillating measurement error
#'   - Weighted means (by signal strength) for each minute interval
#'
#' @import dplyr 
#' @import lubridate
#'
#' @export
#'
locate <- function(data = NULL, det_range = 12, dtime = 1) {
  data <- data %>% filter(!is.na(antBearing))

  # estimate location based on antenna bearing
  tmp <- as.data.frame(.destPoint(
    data$recvDeployLon, data$recvDeployLat,
    data$antBearing, det_range / 2
  ))

  # estimate oscillating error based on antenna bearing
  d <- cbind(data, tmp) %>%
    arrange(ts) %>%
    mutate(
      lon.sd = (det_range / 6) * sin(1 / 90 * pi * antBearing - pi / 2)
        + det_range / 3,
      lat.sd = (det_range / 6) * cos(1 / 90 * pi * antBearing)
        + det_range / 3
    )

  # transform to degrees
  d$lon.sd <- .destPoint(d$lon, d$lat, 90, d$lon.sd)[, 1] - d$lon
  d$lat.sd <- .destPoint(d$lon, d$lat, 0, d$lat.sd)[, 2] - d$lat

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
