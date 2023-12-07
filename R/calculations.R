#' Calculate distances
#'
#' @param x : \code{data.frame}. Data in wide format
#'
#' @return \code{vector}
#'
#' @details
#'
#' Returns distances between consecutive locations in metres.
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
