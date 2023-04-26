stat_density_circular <- function(mapping = NULL, data = NULL,
                                  geom = "area", position = "stack",
                                  ...,
                                  period = 2 * pi,
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
    stat = StatDensityCircular,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      period = period,
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


StatDensityCircular <- ggplot2::ggproto(
  "StatDensityCircular",
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
    
    circular_density <- compute_density_circular(
      x = data$x, w = data$weight,
      period = period, from = range[1], to = range[2],
      bw = bw, adjust = adjust, kernel = kernel, n = n, K = K, bounds = bounds
    )
    circular_density$flipped_aes <- flipped_aes
    flip_data(circular_density, flipped_aes)
  }
)

compute_density_circular <- function(
    x, w, period = 2 * pi,
    from, to, bw = NULL, adjust = 1, kernel = "vonmises", n = 512, K = NULL,
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
  sample_data <- ggplot2:::fit_data_to_bounds(bounds, x, w)
  x <- sample_data$x
  w <- sample_data$w
  nx <- length(x)
  
  # if less than 2 data points, return data frame of NAs and a warning
  if (nx < 2) {
    cli::cli_warn("Groups with fewer than two data points have been dropped.")
    return(
      ggplot2:::data_frame0(
        x = NA_real_,
        density = NA_real_,
        scaled = NA_real_,
        ndensity = NA_real_,
        n = NA_real_,
        .size = 1
      )
    )
  }
  
  # circularize
  x <- circular::as.circular(
    x = 2 * pi * x / period,
    type = "angles",
    units = "radians",
    modulo = "2pi",
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
    dens <- ggplot2:::reflect_density(
      dens = dens, bounds = bounds, from = circular::circular(from * 2 * pi / period), to = circular::circular(to * 2 * pi / period)
    )
  } else {
    dens <- circular::density.circular(
      x = x, bw = bw, adjust = adjust, kernel = kernel, n = n, K = K,
      from = circular::circular(from * 2 * pi / period), to = circular::circular(to * 2 * pi / period)
    )
  }
  
  tibble::tibble(
    x = dens$x * period / (2 * pi),
    density = dens$y * 2 * pi / period,
    scaled = dens$y / max(dens$y, na.rm = TRUE),
    ndensity = dens$y / max(dens$y, na.rm = TRUE),
    count = dens$y * nx * 2 * pi / period,
    n = nx,
    .rows = length(dens$x),
    .name_repair = "minimal"
  )
}