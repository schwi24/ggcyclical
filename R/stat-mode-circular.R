#' Modes of circular density distributions
#'
#' Modes and antimodes are calculated based on local minima and maxima in the
#' circular density distribution.
#' 
#' @seealso [stat_density_circular()]
#' @seealso [stat_mode()]
#' @seealso [circular::density.circular()]
#' @param period The period of the cyclical data. A numeric value with default 
#'  of `2 * pi` radians.
#' @param mode The modes/antimodes to compute. A character string with one of
#'  the following values:
#'  - `mode = "peak"` computes the modes proper (peaks).
#'  - `mode = "valley"` computes the antimodes (valleys/troughs).
#'  - `mode = "trough"` computes the antimodes (valleys/troughs).
#'  - `mode = "both"` computes both modes and antimodes at the same time.
#' @param bw The smoothing bandwidth to be used.
#'  If numeric, the concentration parameter of the circular smoothing kernel.
#'  If character, a rule to choose the bandwidth. The following bandwidth 
#'  selectors are available:
#'  - `bw = "log"` estimates the smoothing bandwidth as `2^log10(n)`. This is 
#'  not optimal for small `n` but quick for large `n`.
#'  - `bw = "taylor"` estimates the smoothing bandwidth with `bw_taylor()`.
#'  - `bw ="nrd.trigmoments"` estimates the smoothing bandwidth with 
#'    [circular::bw.nrd.circular()] and uses `trigmoments` to compute the 
#'    von Mises concentration parameter.
#'  - `bw = "nrd.ml"` estimates the smoothing bandwidth with 
#'   [circular::bw.nrd.circular()] and uses `ML` to compute the von Mises
#'   concentration parameter.
#'  - `bw = "ml"` estimates the smoothing bandwidth with 
#'   [circular::bw.cv.ml.circular()].
#'  - `bw = "mse"` estimates the smoothing bandwidth with 
#'   [circular::bw.cv.mse.circular()].
#'  Currently, wrapped normal distribution kernels are not working because they
#'  require that x is within (0,1).
#'  Note that automatic calculation of the bandwidth does not take weights 
#'  into account.
#' @param adjust A multiplicate bandwidth adjustment. This makes it possible
#'  to adjust the bandwidth while still using the a bandwidth estimator.
#'  For example, `adjust = 1/2` means use half of the default bandwidth.
#' @param kernel Kernel. For circular data, the kernel estimation uses the von
#'  Mises distribution implemented in [circular::density.circular()]. A 
#'  character string with value `kernel = "vonmises"` by default.
#' @param n number of equally spaced points at which the density is to be 
#'  estimated, should be a power of two. Default is `n = 512`).
#' @param K The number of terms used to calculate the wrapped normal 
#'  distribution. This is currently not working.
#' @param trim If `FALSE`, the default, each density is computed on the
#'  full range of the data. If `TRUE`, each density is computed over the
#'  range of that group: this typically means the estimated x values will
#'  not line-up, and hence you won't be able to stack density values.
#'  This parameter only matters if you are displaying multiple densities in
#'  one plot or if you are manually adjusting the scale limits.
#' @param bounds Known lower and upper bounds for estimated data. Default
#'   `c(-Inf, Inf)` means that there are no (finite) bounds. If any bound is
#'   finite, boundary effect of default density estimation will be corrected by
#'   reflecting tails outside `bounds` around their closest edge. Data points
#'   outside of bounds are removed with a warning.
#' @eval ggplot2:::rd_computed_vars(
#'  density  = "circular density estimate at mode points",
#'  count    = "density * number of points - useful for stacked density plots.",
#'  scaled   = "density estimate at mode points, scaled to maximum of 1.",
#'  n        = "number of points.",
#'  ndensity = "alias for `scaled`, to mirror the syntax of [`stat_bin()`]."
#' )
#' @export
#' @rdname geom_mode_circular
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
  
  ggplot2::layer(
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

#' @rdname ggcyclical-extensions
#' @format NULL
#' @usage NULL
#' @export
StatModeCircular <- ggplot2::ggproto(
  "StatModeCircular",
  ggplot2::Stat,
  
  required_aes = c("x|y"),
  
  default_aes = ggplot2::aes(
    x = after_stat(density),
    y = after_stat(density),
    fill = NA,
    weight = NULL
  ),
  
  dropped_aes = "weight",
  
  setup_params = function(self, data, params) {
    params$flipped_aes <- ggplot2::has_flipped_aes(
      data, params, main_is_orthogonal = FALSE, main_is_continuous = TRUE
    )
    
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
    
    data <- ggplot2::flip_data(data, flipped_aes)
    if (trim) {
      range <- range(data$x, na.rm = TRUE)
    } else {
      range <- scales[[ggplot2::flipped_names(flipped_aes)$x]]$dimension()
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
