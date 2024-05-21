#' Motus test data
#'
#' @name motusData
#' @docType data
#' @references \url{https://motus.org/}
#' @keywords data
NULL

# Build data
.buildData <- function(
    data, ID, ts, sig, aLon, aLat, aType, aBearing, det_range) {
  d <- data.frame(
    ID = data[[ID]],
    ts = data[[ts]],
    sig = data[[sig]],
    aLon = data[[aLon]],
    aLat = data[[aLat]],
    aType = data[[aType]],
    aBearing = data[[aBearing]]
  )
  c <- !complete.cases(d)

  if (sum(c) != 0) {
    message(paste(
      "Removed", sum(c), "detections containing missing values."
    ))
  }
  d <- arrange(d[!c, ], ID, ts)

  # Maximum detection range per antenna type
  switch(typeof(det_range),
    double = d$det_range <- det_range,
    list = {
      if (all(d$aType %in% names(det_range))) {
        d$det_range <- unlist(det_range[d$aType])
      } else {
        stop(paste0(
          "Missing detection range for '",
          paste(setdiff(unique(d$aType), names(det_range)), collapse = "', '"),
          "'."
        ))
      }
    },
    stop("'det_range' must be numeric or a named list.")
  )
  return(d)
}
