#' Plot model results
#'
#' @description
#' Plot model results per individual and variable.
#'
#' @param x An object of class `movetrack`.
#' @param vars The variables to plot. Defaults to `c("lon", "lat")`.
#' @param id The individuals to plot. Defaults to `NULL` which plots all
#'   individuals.
#' @param ... Additional arguments passed to `movetrack::summary()`.
#'
#' @return
#' Returns one or multiple `ggplot` plots.
#'
#' @seealso `movetrack::summary()`
#'
#' @examples
#' \dontrun{
#' # Set ggplot theme
#' theme_set(theme_bw(base_size = 20))
#'
#' # Plot
#' plot(fit)
#' plot(fit, vars = "speed", prob = 0.89, ci = "ETI")
#' }
#'
#' @importFrom dplyr filter
#' @importFrom ggplot2 ggplot aes .data geom_segment geom_point ggtitle ylab
#'
#' @method plot movetrack
#' @export
#'
plot.movetrack <- function(x, vars = c("lon", "lat"), id = NULL, ...) {
  if (is.null(id)) {
    id <- unique(x$ID)
  }
  for (i in id) {
    for (var in vars) {
      g <- summary(x, var, ...) |>
        filter(.data$ID == i) |>
        ggplot() +
        geom_segment(aes(
          x = .data$time, y = .data[[paste0(var, ".lower")]],
          xend = .data$time, yend = .data[[paste0(var, ".upper")]]
        ), alpha = 0.2) +
        geom_point(aes(x = .data$time, y = .data[[paste0(var, ".median")]])) +
        ggtitle(paste("ID:", i)) +
        ylab(var)
      plot(g)
    }
  }
}

#' Map model result
#'
#' @description
#' Map individual flight trajectories and model uncertainty.
#'
#' @param fit An object of class `movetrack`.
#' @param id The individuals to plot. Defaults to `NULL` which plots all
#'   individuals.
#' @param nsim The number of posterior draws to plot. Defaults to `50`.
#' @param lwd The line width for the median trajectory. Defaults to `2`.
#' @param alpha The alpha value for the posterior draws. Defaults to `0.1`.
#'
#' @return
#' Returns an overview map with the median trajectories and `nsim` posterior
#'   draws per individual.
#'
#' @examples
#' \dontrun{
#' mapTrack(fit)
#' mapTrack(fit, nsim = 100, alpha = 0.05)
#' }
#'
#' @importFrom ggplot2 ggplot aes .data geom_path labs
#'
#' @export
#'
mapTrack <- function(fit, id = NULL, nsim = 50, lwd = 2, alpha = 0.1) {
  if (is.null(id)) {
    id <- unique(fit$ID)
  }
  g <- fit |>
    as.data.frame() |>
    filter(.data$ID %in% id) |>
    ggplot()
  if (nsim > 0) {
    draws <- getDraws(fit, nsim = nsim)
    g <- g +
      geom_path(
        data = draws |> filter(.data$ID %in% id),
        aes(.data$lon, .data$lat, group = .data$tID), alpha = alpha
      )
  }
  g +
    geom_path(aes(.data$lon, .data$lat, colour = as.factor(.data$ID)),
      lwd = lwd
    ) +
    labs(colour = "ID")
}
