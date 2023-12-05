#' Extract estimates
#'
#' @param fit : \code{stanfit}. Model output
#' @param mdata : \code{data.frame}. Metadata for each time interval
#' @param prob : \code{numeric}. Probability for HDI
#'
#' @return \code{data.frame}
#'
#' @details
#'
#' Returns mean, lower and upper bound of the highest density interval.
#'
#' @import HDInterval
#'
#' @export
#'
estimates <- function(fit, mdata = NULL, prob = 0.9) {
  HDI <- function(x) {
    hdi(c(x), credMass = prob)
  }

  drws <- as.matrix(fit, pars = "y")

  s <- data.frame(
    time = unique(mdata$ts.round),
    name = rep(c("lon", "lat"), each = length(mdata$ts)),
    mean = c(apply(drws, 2, mean)),
    lwr = c(apply(drws, 2, HDI)[1, ]),
    upr = c(apply(drws, 2, HDI)[2, ]),
    motusTagID = rep(mdata$motusTagID, 2),
    speciesEN = rep(mdata$speciesEN, 2)
  )
  return(s)
}