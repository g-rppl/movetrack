# Get means per variable
.getMeans <- function(x) {
  smry <- lapply(x$draws, function(d) apply(d, 3, mean))
  out <- bind_cols(
    ID = x$ID,
    time = x$time,
    do.call(cbind, smry)
  )
  return(as.data.frame(out))
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

#' Summary
#'
#' @description
#' Create a summary of a `movetrack` object.
#'
#' @param object An object of class `movetrack`.
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
#' @importFrom dplyr bind_cols
#' @importFrom HDInterval hdi
#'
#' @method summary movetrack
#' @export
#'
summary.movetrack <- function(
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
  smry <- lapply(object$draws, .summary, ci = ci, prob = prob)
  out <- bind_cols(
    ID = object$ID,
    time = object$time,
    do.call(cbind, smry[var])
  )
  return(as.data.frame(out))
}

#' Print
#'
#' @description
#' Print a summary for a `movetrack` object.
#'
#' @param x An object of class `movetrack`.
#' @param digits The minimal number of *significant* digits; defaults to `3`.
#' @param ... Additional arguments passed to `print()`.
#'
#' @seealso `summary.movetrack()`
#'
#' @importFrom dplyr bind_cols
#'
#' @method print movetrack
#' @export
#'
print.movetrack <- function(x, digits = 3, ...) {
  print(.getMeans(x), digits = digits, ...)
}

#' Coerce to a Data Frame
#'
#' @description
#' Coerce a `movetrack` object to a `data.frame`.
#'
#' @param x An object of class `movetrack`.
#' @param ... Unused; for compatibility with the generic method.
#'
#' @seealso `summary.movetrack()`
#'
#' @importFrom dplyr bind_cols
#'
#' @method as.data.frame movetrack
#' @export
#'
as.data.frame.movetrack <- function(x, ...) {
  return(.getMeans(x))
}

#' Extract draws
#'
#' @description
#' Extract draws from a `movetrack` object.
#'
#' @param fit An object of class `movetrack`.
#' @param nsim The number of simulations to extract; defaults to `50`.
#'
#' @return A `data.frame` with the draws.
#'
#' @importFrom ggplot2 .data
#'
#' @export
#'
getDraws <- function(fit, nsim = 50) {
  # Sample iterations and chains
  it <- sample(dimnames(fit$draws$lon)$iteration, nsim)
  ch <- sample(dimnames(fit$draws$lon)$chain, 1)

  # Build output
  sim <- data.frame(
    ID = rep(fit$ID, each = nsim),
    time = rep(fit$time, each = nsim),
    chain = ch,
    iter = it,
    lon = c(fit$draws$lon[it, ch, ]),
    lat = c(fit$draws$lat[it, ch, ])
  ) |>
    mutate(tID = paste(.data$ID, .data$iter, sep = "_")) |>
    arrange(.data$tID, .data$time)
  return(sim)
}
