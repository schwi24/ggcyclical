#' Modes in density distribution
#'
#' Computes and draws modes (peaks) and antimodes (troughs/valleys) in kernel 
#' density estimates.
#'
#' @eval ggplot2:::rd_orientation()
#' @eval ggplot2:::rd_aesthetics("geom", "point")
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_density
#' @inheritParams ggplot2::geom_point
#' @param geom,stat Use to override the default connection between
#'   `geom_mode_point()` and `stat_mode()`.
#' @seealso [geom_mode_circular()]
#' @export
geom_mode_point <- function(mapping = NULL, data = NULL,
                                  stat = "mode", position = "identity",
                                  ...,
                                  mode = "peak",
                                  bw = "nrd0",
                                  adjust = 1,
                                  kernel = "gaussian",
                                  n = 512,
                                  trim = FALSE,
                                  na.rm = FALSE,
                                  bounds = c(-Inf, Inf),
                                  orientation = NA,
                                  show.legend = NA,
                                  inherit.aes = TRUE
) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomModepoint,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = rlang::list2(
      na.rm = na.rm,
      orientation = orientation,
      ...
    )
  )
}

#' @rdname ggcyclical-extensions
#' @format NULL
#' @usage NULL
#' @export
GeomModepoint <- ggplot2::ggproto(
  "GeomModepoint", ggplot2::GeomPoint,
  required_aes = c("x|y"),
  default_aes = ggplot2::aes(
    fill = NA,
    shape = 19,
    size = 1.5,
    stroke = 0.5,
    colour = "black",
    alpha = NA
  )
)