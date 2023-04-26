stat_mode_circular <- function(mapping = NULL, data = NULL,
                               geom = "point", position = "stack",
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
  
  layer(
    data = data,
    mapping = mapping,
    stat = StatModeCircular,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      period = period,
      mode = mode,
      bw = bw,
      adjust = adjust,
      kernel = kernel,
      n = n,
      K = K,
      trim = trim,
      na.rm = na.rm,
      bounds = bounds,
      orientation = orientation,
      ...
    )
  )
}


StatModeCircular <- ggplot2::ggproto(
  "StatModeCircular",
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
    period = 2 * pi,
    mode = "peak",
    bw = NULL, adjust = 1, kernel = "vonmises", n = 512, K = NULL, trim = FALSE,
    na.rm = FALSE, bounds = c(-Inf, Inf),
    flipped_aes = FALSE
  ) {
    
    data <- flip_data(data, flipped_aes)
    if (trim) {
      range <- range(data$x, na.rm = TRUE)
    } else {
      range <- scales[[flipped_names(flipped_aes)$x]]$dimension()
    }
    
    circular_mode <- compute_density_circular(
      x = data$x, w = data$weight,
      period = period, from = range[1], to = range[2],
      bw = bw, adjust = adjust, kernel = kernel, n = n, K = K, bounds = bounds
    )
    circular_mode <- compute_mode(data = circular_mode, mode = mode)
    
    circular_mode$flipped_aes <- flipped_aes
    flip_data(circular_mode, flipped_aes)
  }
)
