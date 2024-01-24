#' Create a summary of a `stantrackr` object
#'
#' @param object An object of class `stantrackr`.
#' @param ci The method used to calculate the credible intervals. Available
#'   options are `'HDI'` for the highest posterior density interval and `'ETI'`
#'   for the equal-tailed interval; defaults to `'HDI'`.
#' @param prob The probability mass of the credible interval; defaults to `0.9`.
#'
#' @importFrom stats median quantile
#' @importFrom HDInterval hdi
#'
#' @method summary stantrackr
#' @export
#'
summary.stantrackr <- function(object, var = "lon", ci = "HDI", prob = 0.9) {
  if (!ci %in% c("HDI", "ETI")) {
    stop(
      "Invalid credible interval method. Available options are 'HDI' and 'ETI'."
    )
  }
  if (prob < 0 || prob > 1) {
    stop("Probability mass must be between 0 und 1.")
  }
  for (i in seq_len(length(object))) {
    drws <- object[[i]]$draws[[var]]
    s <- data.frame(
      ID = object[[i]]$ID,
      time = object[[i]]$time,
      mean = apply(drws, 3, mean),
      median = apply(drws, 3, median),
      lower = if (ci == "ETI") {
        c(apply(drws, 3, quantile, prob = (1 - prob) / 2))
      } else {
        c(apply(drws, 3, hdi, credMass = prob)[1, ])
      },
      upper = if (ci == "ETI") {
        c(apply(drws, 3, quantile, prob = 1 - (1 - prob) / 2))
      } else {
        c(apply(drws, 3, hdi, credMass = prob)[2, ])
      },
      row.names = NULL
    )

    # Add to output
    if (i == 1) {
      summary <- s
    } else {
      summary <- rbind(summary, s)
    }
  }
  return(summary)
}

# Get means per variable
.getMeans <- function(x) {
  lon <- summary(x, var = "lon")
  lat <- summary(x, var = "lat")
  distance <- summary(x, var = "distance")
  speed <- summary(x, var = "speed")
  s <- data.frame(
    ID = lon$ID,
    time = lon$time,
    lon = lon$mean,
    lat = lat$mean,
    distance = distance$mean,
    speed = speed$mean
  )
  return(s)
}

#' Print a summary for a `stantrackr` object
#'
#' @param x An object of class `stantrackr`.
#' @param digits The minimal number of *significant* digits; defaults to `3`.
#'
#' @seealso `\link{summary.stantrackr}`
#'
#' @method print stantrackr
#' @export
#'
print.stantrackr <- function(x, digits = 3) {
  print(.getMeans(x), digits = digits)
}

#' Coerce a `stantrackr` object to a Data Frame
#'
#' @param x An object of class `stantrackr`.
#'
#' @method as.data.frame stantrackr
#' @export
#'
as.data.frame.stantrackr <- function(x) {
  return(.getMeans(x))
}
