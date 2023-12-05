#' Extract estimates
#'
#' @description
#' Extract estimates from model output.
#'
#' @param fit `stanfit` Model output
#' @param mdata `data.frame` Metadata for each time interval
#' @param prob `numeric` Probability for HDI
#'
#' @return a `data.frame`
#'
#' @details
#'
#' Returns mean, lower and upper bound of the highest density interval.
#'
#' @import cmdstanr
#' @importFrom HDInterval hdi
#'
#' @export
#'
estimates <- function(fit, mdata = NULL, prob = 0.9) {
  HDI <- function(x) {
    hdi(c(x), credMass = prob)
  }

  drws <- fit$draws("y")

  s <- data.frame(
    time = unique(mdata$ts.round),
    name = rep(c("lon", "lat"), each = length(mdata$ts)),
    mean = c(apply(drws, 3, mean)),
    lwr = c(apply(drws, 3, HDI)[1, ]),
    upr = c(apply(drws, 3, HDI)[2, ]),
    motusTagID = rep(mdata$motusTagID, 2),
    speciesEN = rep(mdata$speciesEN, 2)
  )
  return(s)
}