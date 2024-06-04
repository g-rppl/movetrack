#' Motus test data
#'
#' @name motusData
#' @docType data
#' @references \url{https://motus.org/}
#' @keywords data
"motusData"

# Build data
.buildData <- function(
    data, ID, ts, sig, aLon, aLat, aType, aBearing, aRange) {
  # Check for missing columns
  i <- !c(ID, ts, sig, aLon, aLat, aBearing) %in% names(data)
  if (any(i)) {
    stop(paste0(
      "Missing columns: '",
      paste(c(ID, ts, sig, aLon, aLat, aBearing)[i],
        collapse = "', '"
      ),
      "'."
    ))
  }
  d <- data.frame(
    ID = data[[ID]],
    ts = data[[ts]],
    sig = data[[sig]],
    aLon = data[[aLon]],
    aLat = data[[aLat]],
    aType = if (is.list(aRange)) {
      if (aType %in% names(data) && !is.null(aType)) {
        data[[aType]]
      } else {
        stop("Missing 'aType' column.")
      }
    } else "any",
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
  switch(typeof(aRange),
    double = d$aRange <- aRange,
    list = {
      if (all(d$aType %in% names(aRange))) {
        d$aRange <- unlist(aRange[d$aType])
      } else {
        stop(paste0(
          "Missing detection range for '",
          paste(setdiff(unique(d$aType), names(aRange)), collapse = "', '"),
          "'."
        ))
      }
    },
    stop("'aRange' must be numeric or a named list.")
  )
  return(d)
}
