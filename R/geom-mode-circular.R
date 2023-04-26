geom_mode_circular <- function(mapping = NULL, data = NULL,
                                  stat = "mode_circular", position = "identity",
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
