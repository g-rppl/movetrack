#' Extract estimates
#'
#' @description
#' Extract estimates from model output.
#'
#' @param fit A `CmdStanMCMC` object containing the model output.
#' @param mdata A `data.frame` containing metadata for each time interval.
#' @param prob A scalar `[0, 1]` specifying the mass within the credible
#'   interval.
#'
#' @return
#' Returns a `data.frame` containing the mean and lower and upper bound of the
#' highest density interval for each time interval.
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
    time = unique(mdata$ts),
    name = rep(c("lon", "lat"), each = length(mdata$ts)),
    mean = c(apply(drws, 3, mean)),
    lwr = c(apply(drws, 3, HDI)[1, ]),
    upr = c(apply(drws, 3, HDI)[2, ])
  )
  return(s)
}
