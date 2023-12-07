# Build data
.buildData <- function(data, ts, sig, aLon, aLat, aBearing) {
  d <- data.frame(
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
  return(arrange(d[!c, ], ts))
}
