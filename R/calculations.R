#' Calculate distances
#'
#' @description
#' Calculate distances between consecutive locations.
#'
#' @param x A `data.frame` in wide format.
#'
#' @return
#' Returns a vector of distances between consecutive locations in metres.
#'
#' @export
#'
distance <- function(x) {
  dist <- rep(NA, nrow(x))
  for (i in 2:nrow(x)) {
    dist[i] <- .distGeo(x$lon[i], x$lat[i], x$lon[i - 1], x$lat[i - 1])
  }
  return(dist * 1e3)
}

#' Calculate speed
#'
#' @description
#' Calculate flight speed between consecutive locations.
#'
#' @param x A `data.frame` in wide format.
#'
#' @return 
#' Returns a vector of speeds between consecutive locations in m/s.
#'
#' @import lubridate
#'
#' @export
#'
speed <- function(x) {
  spd <- distance(x) / (c(NA, diff(x$time)) * 60)
  return(spd)
}
