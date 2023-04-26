geom_density_circular <- function(mapping = NULL, data = NULL,
                         stat = "density_circular", position = "identity",
                         ...,
                         na.rm = FALSE,
                         orientation = NA,
                         show.legend = NA,
                         inherit.aes = TRUE
                         ) {
  layer(
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

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' @include geom-ribbon.R
GeomDensityCircular <- ggproto(
  "GeomDensityCircular", GeomDensity,
  default_aes = aes(
    fill = NA,
    linewidth = 0.5,
    linetype = 1,
    weight = 1,
    colour = "black",
    alpha = NA
  )
)

