geom_mode_point <- function(mapping = NULL, data = NULL,
                                  stat = "mode", position = "identity",
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

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomModepoint <- ggproto(
  "GeomModepoint", GeomPoint,
  default_aes = aes(
    fill = NA,
    shape = 19,
    size = 1.5,
    stroke = 0.5,
    colour = "black",
    alpha = NA
  )
)