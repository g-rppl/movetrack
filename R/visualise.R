#' Plot model results
#'
#' @description
#' Plot model results per individual and variable.
#'
#' @param fit An object of class `stantrackr`.
#' @param vars The variables to plot. Defaults to `c("lon", "lat")`.
#' @param id The individuals to plot. Defaults to `NULL` which plots all
#'   individuals.
#' @param ... Additional arguments passed to `stantrckr::summary()`.
#'
#' @return
#' Returns one or multiple `ggplot` plots.
#'
#' @seealso `stantrackr::summary()`
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
#' @importFrom dplyr %>% filter
#' @importFrom ggplot2 ggplot aes geom_segment geom_point ggtitle
#'
#' @method plot stantrackr
#' @export
#'
plot.stantrackr <- function(fit, vars = c("lon", "lat"), id = NULL, ...) {
  if (is.null(id)) {
    id <- unique(fit$ID)
  }
  for (i in id) {
    for (var in vars) {
      g <- summary(fit, var, ...) %>%
        filter(ID == i) %>%
        ggplot() +
        geom_segment(aes(
          x = time, y = .data[[paste0(var, ".lower")]],
          xend = time, yend = .data[[paste0(var, ".upper")]]
        ), alpha = 0.2) +
        geom_point(aes(x = time, y = .data[[paste0(var, ".mean")]])) +
        ggtitle(paste("ID:", i))
      plot(g)
    }
  }
}

#' Map model result
#'
#'
