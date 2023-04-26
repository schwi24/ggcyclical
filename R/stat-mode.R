stat_mode <- function(mapping = NULL, data = NULL,
                               geom = "point", position = "stack",
                               ...,
                               period = 2 * pi,
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
      period = period,
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


StatMode <- ggproto(
  "StatMode",
  Stat,
  
  required_aes = c("x|y"),
  
  default_aes = aes(x = after_stat(density), y = after_stat(density), fill = NA, weight = NULL),
  
  dropped_aes = "weight",
  
  setup_params = function(self, data, params) {
    params$flipped_aes <- has_flipped_aes(data, params, main_is_orthogonal = FALSE, main_is_continuous = TRUE)
    
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
    
    data <- flip_data(data, flipped_aes)
    if (trim) {
      range <- range(data$x, na.rm = TRUE)
    } else {
      range <- scales[[flipped_names(flipped_aes)$x]]$dimension()
    }
    
    modal <- compute_mode(
      x = data$x, w = data$weight,
      mode = mode, from = range[1], to = range[2],
      bw = bw, adjust = adjust, kernel = kernel, n = n, bounds = bounds
    )
    modal$flipped_aes <- flipped_aes
    flip_data(modal, flipped_aes)
  }
)

compute_mode <- function(x, w, mode = "peak", from, to, bw = "nrd0", adjust = 1, kernel = "gaussian", n = 512, bounds = c(-Inf, Inf)) {
  
  nx <- length(x)
  if (is.null(w)) {
    w <- rep(1/nx, nx)
  } else {
    w <- w / sum(w)
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
  # if selected mode invalid, choose default and return warning
  if (!(mode %in% c("peak", "trough", "both"))) {
    cli::cli_warn("Mode defaulted to peak because of invalid value.")
    mode <- "peak"
  }
  
  # Decide whether to use boundary correction
  if (any(is.finite(bounds))) {
    dens <- stats::density(
      x = x, weights = w, bw = bw, adjust = adjust, kernel = kernel, n = n,
    )
    dens <- ggplot2:::reflect_density(
      dens = dens, bounds = bounds, from = from, to = to
    )
  } else {
    dens <- stats::density(
      x = x, weights = w, bw = bw, adjust = adjust, kernel = kernel, n = n,
      from = from, to = to
    )
  }
  
  # Extract coordinates of modes
  sel <- switch(
    mode,
    "peak" = which(diff(sign(diff(dens$y))) < 0) + 1,
    "trough" = which(diff(sign(diff(dens$y))) > 0) + 1,
    "both" = c(which(diff(sign(diff(dens$y))) < 0) + 1, which(diff(sign(diff(dens$y))) > 0) + 1)
  )
  
  ggplot2:::data_frame0(
    x = dens$x[sel],
    density = dens$y[sel],
    scaled = dens$y[sel] / max(dens$y, na.rm = TRUE),
    ndensity = dens$y[sel] / max(dens$y, na.rm = TRUE),
    count = dens$y[sel] * nx,
    n = nx,
    .size = length(dens$x[sel])
  )
}