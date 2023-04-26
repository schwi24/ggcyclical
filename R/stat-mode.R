stat_mode <- function(mapping = NULL, data = NULL,
                               geom = "point", position = "stack",
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
                               inherit.aes = TRUE) {
  
  layer(
    data = data,
    mapping = mapping,
    stat = StatMode,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      mode = mode,
      bw = bw,
      adjust = adjust,
      kernel = kernel,
      n = n,
      trim = trim,
      na.rm = na.rm,
      bounds = bounds,
      orientation = orientation,
      ...
    )
  )
}


StatMode <- ggplot2::ggproto(
  "StatMode",
  ggplot2::Stat,
  
  required_aes = c("x|y"),
  
  default_aes = ggplot2::aes(x = after_stat(density), y = after_stat(density), fill = NA, weight = NULL),
  
  dropped_aes = "weight",
  
  setup_params = function(self, data, params) {
    params$flipped_aes <- ggplot2::has_flipped_aes(data, params, main_is_orthogonal = FALSE, main_is_continuous = TRUE)
    
    has_x <- !(is.null(data$x) && is.null(params$x))
    has_y <- !(is.null(data$y) && is.null(params$y))
    if (!has_x && !has_y) {
      cli::cli_abort("{.fn {snake_class(self)}} requires an {.field x} or {.field y} aesthetic.")
    }
    
    return(params)
  },
  
  extra_params = c("na.rm", "orientation"),
  
  compute_group = function(
    data, scales,
    mode = "peak",
    bw = "nrd0", adjust = 1, kernel = "gaussian", n = 512, trim = FALSE,
    na.rm = FALSE, bounds = c(-Inf, Inf),
    flipped_aes = FALSE
  ) {
    
    data <- ggplot2::flip_data(data, flipped_aes)
    if (trim) {
      range <- range(data$x, na.rm = TRUE)
    } else {
      range <- scales[[flipped_names(flipped_aes)$x]]$dimension()
    }
    
    modal <- ggplot2:::compute_density(
      data$x, data$weight, from = range[1], to = range[2],
      bw = bw, adjust = adjust, kernel = kernel, n = n, bounds = bounds
    )
    modal <- compute_mode(data = modal, mode = mode)

    modal$flipped_aes <- flipped_aes
    ggplot2::flip_data(modal, flipped_aes)
  }
)

compute_mode <- function(data, mode) {

  modal_positions <- switch(
    mode,
    "peak" = which(diff(sign(diff(data$density))) < 0) + 1,
    "trough" = which(diff(sign(diff(data$density))) > 0) + 1,
    "both" = c(
      which(diff(sign(diff(data$density))) < 0) + 1,
      which(diff(sign(diff(data$density))) > 0) + 1
    )
  )

  tibble::tibble(
    x = data$x[modal_positions],
    density = data$density[modal_positions],
    scaled = data$scaled[modal_positions],
    ndensity = data$ndensity[modal_positions],
    count = data$count[modal_positions],
    n = data$n[modal_positions],
    .rows = length(data$x[modal_positions]),
    .name_repair = "minimal"
  )

}