#' Smoothed circular density estimates
#' 
#' @seealso [circular::density.circular()]
#' @seealso [bw_taylor()]
#' @seealso [circular::bw.nrd.circular()]
#' @seealso [circular::bw.cv.ml.circular()]
#' @seealso [circular::bw.cv.mse.circular()]
#' @param period The period of the cyclical data. A numeric value with default 
#'  of `2 * pi` radians.
#' @param bw The smoothing bandwidth to be used.
#'  If numeric, the concentration parameter of the circular smoothing kernel.
#'  If character, a rule to choose the bandwidth. The following bandwidth 
#'  selectors are available:
#'  - `bw = "log"` estimates the smoothing bandwidth as `2^log10(n)`. This is 
#'    not optimal for small `n` but quick for large `n`. This is the default.
#'  - `bw = "taylor"` estimates the smoothing bandwidth with `bw_taylor()`.
#'  - `bw = "nrd.trigmoments"` estimates the smoothing bandwidth with 
#'    [circular::bw.nrd.circular()] and uses `trigmoments` to compute the 
#'    von Mises concentration parameter.
#'  - `bw = "nrd.ml"` estimates the smoothing bandwidth with 
#'    [circular::bw.nrd.circular()] and uses `ML` to compute the von Mises
#'    concentration parameter.
#'  - `bw = "ml"` estimates the smoothing bandwidth with 
#'    [circular::bw.cv.ml.circular()].
#'  - `bw = "mse"` estimates the smoothing bandwidth with 
#'    [circular::bw.cv.mse.circular()].
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
#'  `c(-Inf, Inf)` means that there are no (finite) bounds. If any bound is
#'  finite, boundary effect of default density estimation will be corrected by
#'  reflecting tails outside `bounds` around their closest edge. Data points
#'  outside of bounds are removed with a warning.
#' @eval rd_computed_vars(
#'  density  = "circular density estimate.",
#'  count    = "density * number of points - useful for stacked density plots.",
#'  scaled   = "density estimate, scaled to maximum of 1.",
#'  n        = "number of points.",
#'  ndensity = "alias for `scaled`, to mirror the syntax of [`stat_bin()`]."
#' )
#' @export
#' @rdname geom_density_circular
stat_density_circular <- function(mapping = NULL, data = NULL,
                                  geom = "area", position = "stack",
                                  ...,
                                  period = 2 * pi,
                                  bw = "log",
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
    stat = StatDensityCircular,
    geom = geom,
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

#' @rdname ggcyclical-extensions
#' @format NULL
#' @usage NULL
#' @export
StatDensityCircular <- ggplot2::ggproto(
  "StatDensityCircular",
  ggplot2::Stat,
  
  required_aes = c("x|y"),
  
  default_aes = ggplot2::aes(
    x = after_stat(density),
    y = after_stat(density),
    fill = NA, weight = NULL
  ),
  
  dropped_aes = "weight",
  
  setup_params = function(self, data, params) {
    params$flipped_aes <- ggplot2::has_flipped_aes(
      data,
      params,
      main_is_orthogonal = FALSE,
      main_is_continuous = TRUE
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
    bw = "log", adjust = 1, kernel = "vonmises", n = 512, K = NULL, trim = FALSE,
    na.rm = FALSE, bounds = c(-Inf, Inf),
    flipped_aes = FALSE
  ) {
    
    data <- ggplot2::flip_data(data, flipped_aes)
    if (trim) {
      range <- range(data$x, na.rm = TRUE)
    } else {
      range <- scales[[ggplot2::flipped_names(flipped_aes)$x]]$dimension()
    }
    
    circular_density <- compute_density_circular(
      x = data$x, w = data$weight,
      period = period, from = range[1], to = range[2],
      bw = bw, adjust = adjust, kernel = kernel, n = n, K = K, bounds = bounds
    )
    circular_density$flipped_aes <- flipped_aes
    ggplot2::flip_data(circular_density, flipped_aes)
  }
)

compute_density_circular <- function(
    x, w,
    period = 2 * pi, from = 0, to = 2 * pi, 
    bw = "log", adjust = 1, kernel = "vonmises", n = 512, K = NULL,
    bounds = c(-Inf, Inf)
  ) {
  
  #Extend circular data ± period
  nx <- length(x)
  if (is.null(w)) {
    w <- rep(1/nx, nx)
  } else {
    w <- w/sum(w)
  }
  
  # Adjust data points and weights to all fit inside bounds
  sample_data <- fit_data_to_bounds(bounds, x, w)
  x <- sample_data$x
  w <- sample_data$w
  nx <- length(x)
  
  # if less than 2 data points, return data frame of NAs and a warning
  if (nx < 2) {
    cli::cli_warn("Groups with fewer than two data points have been dropped.")
    return(
      tibble::tibble(
        x = NA_real_,
        density = NA_real_,
        scaled = NA_real_,
        ndensity = NA_real_,
        n = NA_real_,
        .rows = 1,
        .name_repair = "minimal"
      )
    )
  }
  
  # circularize
  x <- circular::circular(
    x = 2 * pi * x / period,
    type = "angles",
    units = "radians",
    modulo = "2pi",
    zero = 0,
    rotation = "clock",
    template = "none"
  )
  from <- circular::circular(
    x = from * 2 * pi / period,
    type = "angles",
    units = "radians",
    modulo = "asis",
    zero = 0,
    rotation = "clock",
    template = "none"
  )
  
  to <- circular::circular(
    x = to * 2 * pi / period,
    type = "angles",
    units = "radians",
    modulo = "asis",
    zero = 0,
    rotation = "clock",
    template = "none"
  )

  # Calculate bandwidth
  if(!is.null(bw)){
    bw <- switch(
      as.character(bw),
      taylor = bw_taylor(x = x, kappa.est = NULL),
      nrd.trigmoments = circular::bw.nrd.circular(
        x = x, lower=NULL, upper=NULL,
        kappa.est="trigmoments", kappa.bias=TRUE, P=3
      ),
      nrd.ml = circular::bw.nrd.circular(
        x = x, lower=NULL, upper=NULL,
        kappa.est="ML", kappa.bias=TRUE, P=3
      ),
      ml = circular::bw.cv.ml.circular(
        x = x, lower=NULL, upper=NULL, tol = 1e-4,
        kernel = kernel, K = K, min.k = 10
      ),
      mse = circular::bw.cv.mse.circular(
        x = x, lower=NULL, upper=NULL, tol = 1e-4,
        kernel = kernel, K = K, min.k = 10
      ),
      log = 2^log10(nx),
      as.numeric(bw)
    )
  } else {
    bw <- 2^log10(nx)
  }
  
  # Decide whether to use boundary correction
  if (any(is.finite(bounds))) {
    dens <- circular::density.circular(
      x = x, bw = bw, adjust = adjust, kernel = kernel, n = n, K = K
    )
    dens <- reflect_density(
      dens = tibble::tibble(x = as.numeric(dens$x), y = as.numeric(dens$y)),
      bounds = bounds,
      from = as.numeric(from),
      to = as.numeric(to)
    )
  } else {
    dens <- circular::density.circular(
      x = x, bw = bw, adjust = adjust, kernel = kernel, n = n, K = K,
      from = from,
      to = to
    )
  }
  # decircularize
  dens <- tibble::tibble(
    x = (as.numeric(dens$x) * period / (2 * pi)),
    y = (as.numeric(dens$y) * 2 * pi / period)
  )
  
  tibble::tibble(
    x = dens$x,
    density = dens$y,
    scaled = dens$y / max(dens$y, na.rm = TRUE),
    ndensity = dens$y / max(dens$y, na.rm = TRUE),
    count = dens$y * nx,
    n = nx,
    .rows = length(dens$x),
    .name_repair = "minimal"
  )
}