# Get means per variable
.getMeans <- function(x) {
  summary <- lapply(x, function(obj) {
    drws <- obj$draws
    smry <- lapply(drws, function(d) apply(d, 3, mean))
    data.frame(
      ID = obj$ID,
      time = obj$time,
      do.call(cbind, smry)
    )
  })
  summary <- do.call(rbind, summary)
  return(data.frame(summary, row.names = NULL))
}

# Summarise draws
.summary <- function(drws, ci = "HDI", prob = 0.9) {
  s <- data.frame(
    mean = apply(drws, 3, mean),
    median = apply(drws, 3, median),
    lower = if (ci == "ETI") {
      c(apply(drws, 3, quantile, prob = (1 - prob) / 2, na.rm = TRUE))
    } else {
      c(apply(drws, 3, hdi, credMass = prob)[1, ])
    },
    upper = if (ci == "ETI") {
      c(apply(drws, 3, quantile, prob = 1 - (1 - prob) / 2, na.rm = TRUE))
    } else {
      c(apply(drws, 3, hdi, credMass = prob)[2, ])
    },
    row.names = NULL
  )
  return(s)
}

#' Create a summary of a `stantrackr` object
#'
#' @param object An object of class `stantrackr`.
#' @param var The variable to summarise; defaults to `'lon'`.
#' @param ci The method used to calculate the credible intervals. Available
#'   options are `'HDI'` for the highest posterior density interval and `'ETI'`
#'   for the equal-tailed interval; defaults to `'HDI'`.
#' @param prob The probability mass of the credible interval; defaults to `0.9`.
#' @param ... Unused; for compatibility with the generic method.
#'
#' @return A `data.frame` with the summary statistics.
#'
#' @examples
#' \dontrun{
#' summary(fit)
#' summary(fit, var = "distance")
#' summary(fit, ci = "ETI", prob = 0.89)
#' }
#'
#' @importFrom stats median quantile
#' @importFrom dplyr bind_rows
#' @importFrom HDInterval hdi
#'
#' @method summary stantrackr
#' @export
#'
summary.stantrackr <- function(
    object,
    var = c("lon", "lat"),
    ci = "HDI",
    prob = 0.9,
    ...) {
  # Check inputs
  if (!any(var %in% c("lon", "lat", "distance", "speed"))) {
    stop(paste(
      "Invalid variable. Available options are 'lon', 'lat', 'distance', and",
      "'speed'."
    ))
  }
  if (!ci %in% c("HDI", "ETI")) {
    stop(
      "Invalid credible interval method. Available options are 'HDI' and 'ETI'."
    )
  }
  if (prob < 0 || prob > 1) {
    stop("Probability mass must be between 0 und 1.")
  }
  summary <- lapply(object, function(obj) {
    drws <- obj$draws
    smry <- lapply(drws, .summary, ci = ci, prob = prob)
    cbind(
      ID = obj$ID,
      time = obj$time,
      do.call(cbind, smry[var])
    )
  })
  return(bind_rows(summary))
}

#' Print a summary for a `stantrackr` object
#'
#' @param x An object of class `stantrackr`.
#' @param digits The minimal number of *significant* digits; defaults to `3`.
#' @param ... Additional arguments passed to `print()`.
#'
#' @seealso `summary.stantrackr()`
#'
#' @method print stantrackr
#' @export
#'
print.stantrackr <- function(x, digits = 3, ...) {
  print(.getMeans(x), digits = digits, ...)
}

#' Coerce a `stantrackr` object to a Data Frame
#'
#' @param x An object of class `stantrackr`.
#' @param ... Unused; for compatibility with the generic method.
#'
#' @seealso `summary.stantrackr()`
#'
#' @method as.data.frame stantrackr
#' @export
#'
as.data.frame.stantrackr <- function(x, ...) {
  return(.getMeans(x))
}
