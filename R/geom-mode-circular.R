#' Modes in circular density distribution
#'
#' Computes and draws modes (peaks) and antimodes (troughs/valleys) in circular
#' density estimates. This is a specific version of geom_mode_point for circular
#' density distribution.
#'
#' @eval rd_orientation()
#' @eval rd_aesthetics("geom", "point")
#' @inheritParams ggplot2::layer
#' @inheritParams geom_density_circular
#' @inheritParams geom_mode_point
#' @seealso [geom_mode_point]
#' @param geom,stat Use to override the default connection between
#'   `geom_mode_circular()` and `stat_mode_circular()`.
#' @export
geom_mode_circular <- function(mapping = NULL, data = NULL,
                               stat = "mode_circular", position = "identity",
                               ...,
                               period = 2 * pi,
                               mode = "peak",
                               bw = NULL,
                               adjust = 1,
                               kernel = "vonmises",
                               n = 512,
                               K = NULL,
                               trim = FALSE,
                               na.rm = FALSE,
                               bounds = c(-Inf, Inf),
                               orientation = NA,
                               show.legend = NA,
                               inherit.aes = TRUE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomModepoint,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = rlang::list2(
      period = period,
      bw = bw,
      adjust = adjust,
      kernel = kernel,
      n = n,
      K = K,
      trim = trim,
      na.rm = na.rm,
      orientation = orientation,
      bounds = bounds,
      ...
    )
  )
}
