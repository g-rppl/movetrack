#' Motus test data
#'
#' @name motusData
#' @docType data
#' @references \url{https://motus.org/}
#' @keywords data
NULL

# Build data
.buildData <- function(data, ID, ts, sig, aLon, aLat, aBearing) {
  d <- data.frame(
    ID = data[[ID]],
    ts = data[[ts]],
    sig = data[[sig]],
    aLon = data[[aLon]],
    aLat = data[[aLat]],
    aBearing = data[[aBearing]]
  )
  c <- !complete.cases(d)

  if (sum(c) != 0) {
    message(paste(
      "Removed", sum(c), "detections containing missing values."
    ))
  }
  return(arrange(d[!c, ], ID, ts))
}
