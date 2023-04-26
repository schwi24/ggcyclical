#' Smoothed circular density estimates
#'
#' Computes and draws circular kernel density estimate. This is a useful 
#' alternative to regular kernel density estimate, which fail to capture
#' cyclical (circular, periodic, directional, polar) data properly.
#'
#' @eval ggplot2:::rd_orientation()
#' @eval ggplot2:::rd_aesthetics("geom", "density")
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_density
#' @param geom,stat Use to override the default connection between
#'   `geom_density_circular()` and `stat_density_circular()`.
#' @export
geom_density_circular <- function(mapping = NULL, data = NULL,
                         stat = "density_circular", position = "identity",
                         ...,
                         period = 2 * pi,
                         bw = NULL,
                         adjust = 1,
                         kernel = "vonmises",
                         n = 512,
                         K = NULL,
                         trim = FALSE,
                         bounds = c(-Inf, Inf),
                         na.rm = FALSE,
                         orientation = NA,
                         show.legend = NA,
                         inherit.aes = TRUE
                         ) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomDensityCircular,
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
GeomDensityCircular <- ggplot2::ggproto(
  "GeomDensityCircular", ggplot2::GeomDensity,
  required_aes = c("x|y"),
  default_aes = ggplot2::aes(
    fill = NA,
    linewidth = 0.5,
    linetype = 1,
    weight = 1,
    colour = "black",
    alpha = NA
  )
)

