#' Modes of density distributions
#'
#' Modes and antimodes are calculated based on local minima and maxima in the
#' density distribution.
#' 
#' @seealso [ggplot2::stat_density()]
#' @seealso [stat_mode_circular()]
#' @param mode The modes/antimodes to compute. A character string with one of
#'   the following values:
#'   - `mode = "peak"` computes the modes proper (peaks).
#'   - `mode = "valley"` computes the antimodes (valleys/troughs).
#'   - `mode = "trough"` computes the antimodes (valleys/troughs).
#'   - `mode = "both"` computes both modes and antimodes at the same time.
#' #' @param bw The smoothing bandwidth to be used.
#'   If numeric, the standard deviation of the smoothing kernel.
#'   If character, a rule to choose the bandwidth, as listed in
#'   [stats::bw.nrd()]. Note that automatic calculation of the bandwidth does
#'   not take weights into account.
#' @param adjust A multiplicate bandwidth adjustment. This makes it possible
#'   to adjust the bandwidth while still using the a bandwidth estimator.
#'   For example, `adjust = 1/2` means use half of the default bandwidth.
#' @param kernel Kernel. See list of available kernels in [stats::density()].
#' @param n number of equally spaced points at which the density is to be
#'   estimated, should be a power of two, see [stats::density()] for
#'   details
#' @param trim If `FALSE`, the default, each density is computed on the
#'   full range of the data. If `TRUE`, each density is computed over the
#'   range of that group: this typically means the estimated x values will
#'   not line-up, and hence you won't be able to stack density values.
#'   This parameter only matters if you are displaying multiple densities in
#'   one plot or if you are manually adjusting the scale limits.
#' @param bounds Known lower and upper bounds for estimated data. Default
#'   `c(-Inf, Inf)` means that there are no (finite) bounds. If any bound is
#'   finite, boundary effect of default density estimation will be corrected by
#'   reflecting tails outside `bounds` around their closest edge. Data points
#'   outside of bounds are removed with a warning.
#' @eval ggplot2:::rd_computed_vars(
#'   density  = "circular density estimate at mode points",
#'   count    = "density * number of points - useful for stacked density plots.",
#'   scaled   = "density estimate at mode points, scaled to maximum of 1.",
#'   n        = "number of points.",
#'   ndensity = "alias for `scaled`, to mirror the syntax of [`stat_bin()`]."
#' )
#' @export
#' @rdname geom_mode_point
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
  
  ggplot2::layer(
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

#' @rdname ggcyclical-extensions
#' @format NULL
#' @usage NULL
#' @export
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
      range <- scales[[ggplot2::flipped_names(flipped_aes)$x]]$dimension()
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
    "valley" = which(diff(sign(diff(data$density))) > 0) + 1,
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