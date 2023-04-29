#' Circular measures of location
#' 
#' Circular data repeat after every period, which is challenging for 
#' calculating location (mean) and dispersion measures (standard deviation).
#' 
#' This package implements the following measures of location for circular data.
#' @family location_measures
#' @name circular_location
NULL


#' Circular measures of spread
#' 
#' Circular data repeat after every period, which is challenging for 
#' calculating location (mean) and dispersion measures (standard deviation).
#' 
#' This package implements the following measures of spread for circular data.
#' @family spread_measures
#' @name circular_spread
NULL


#' Nice angles
#' 
#' The function `nice_angles` converts cyclical data to the standard interval 
#' `[0, period[` or unit cycle `[0, 2*pi[` to simplify calculations.
#' @param x Numeric vector
#' @param period A numeric value declaring the period of the circular data. The
#' default is `2 * pi`.
#' @param force_radians A boolean value deciding if the conversion is to the 
#' unit cycle. The default value is `TRUE`, converting the output on the unit 
#' cycle `[0, 2*pi[`.
#' @return A numeric vector with circular data converted to the range 
#' `[0, period[`.
#' @export
nice_angles <- function(x, period = 2*pi, force_radians = TRUE) {

  # Check input types
  if (!all(is.numeric(x), is.numeric(period), is.logical(force_radians))) {
    stop("Invalid values! `x` and `period` must be numeric. `force_radiance` must be locial.")
  }
  
  nice <- x %% period
  if(force_radians){
    nice <- nice * 2 * pi / period
  }
  
  return(nice)
  
}

#' Angular range check
#' 
#' This function checks if angles are within a specified range. The angles `x`
#' and the test range `angular_range` need to be in the same units but can be
#' any circular revolution of the angles ( `a = a + period * n` ).
#' @param x Numeric vector with angles to be tested.
#' @param period A numeric value declaring the period of the circular data. The
#' default is `2 * pi`.
#' @param angular_range Numeric vector with only the limits of
#' the angular test range. The full circle range in radians, `angular_range = c(0, 2*pi)`,
#' is chosen by default.
#' @return A logical vector of same length as x with values `TRUE` if the 
#' individual angle is within the test range or `FALSE` if not.
#' @export
is_angle_in_range <- function(x, period = 2*pi, angular_range = c(0, 2*pi)) {
  # Test if an angle x falls within angular_range
  
  # Check input types
  if (!all(is.numeric(x), is.numeric(period), is.numeric(angular_range))) {
    stop("Invalid values! `x`, `period`, and `angular_range` must be numeric.")
  }
  
  # Convert angles to radians
  theta <- 2 * pi * x / period
  
  start <- nice_angles(angular_range[1], period = period, force_radians = TRUE)
  end <- nice_angles(angular_range[2], period = period, force_radians = TRUE)
  
  # Check whether angle is interal range or external range
  if(start <= end) {
    test <- (theta >= start) & (end > theta)
  } else {
    test1 <- (theta >= 0) & (end > theta)
    test2 <- (theta >= start) & ((2*pi) > theta)
    test <- test1 | test2
  }
  
  return(test)
}


#' Angular mean
#' 
#' Circular data repeat every period which has to be taken into account
#' for calculating the averages. 
#' 
#' @details
#' After transformation of the data to radians on the unit circle, the angular 
#' arithmetic mean can be calculated as follows:
#' 
#' \deqn{\bar{\theta} = \text{atan2}\left(\frac{1}{n}\sum_{i=1}^n\text{sin}
#' (\theta_i),\frac{1}{n}\sum_{i=1}^n\text{cos}(\theta_i)\right)}
#' 
#' Because of the periodicity of circular data, the circular mean can be offset 
#' by \eqn{n} revolutions such that \eqn{\bar{\theta} = \bar{\theta} 
#' + 2\pi n}. This can be controlled with the parameter `revolution`.
#' 
#' @param x Numeric vector with circular data.
#' @param period A numeric value declaring the period of the circular data. The
#' default is `2 * pi`.
#' @param revolution A numeric value indicating the number of revolutions to 
#' add to the circular mean. The default is `revolution = 0`, which keeps the 
#' circular mean in the standard interval `[0, period[`. If `revolution = NULL`,
#' the mean revolution is added.
#' @return The average numeric value of the circular data.
#' @family location_measures
#' @export
angular_mean <- function(x, period = 2*pi, revolution = 0) {
  
  # Check input types
  if (!all(is.numeric(x), is.numeric(period), is.numeric(revolution))) {
    stop("Invalid values! `x`, `period`, and `revolution` must be numeric.")
  }
  
  # Convert to radians and count revolutions
  theta <- 2 * pi * x / period
  if(is.null(revolution)|!is.numeric(revolution)){
    revolution <- floor(mean(floor(theta/(2*pi))))
  }

  # Calculate mean angle
  sx <- mean(sin(theta))
  cx <- mean(cos(theta))
  cmean <- atan2(sx, cx)
  cmean <- nice_angles(cmean)
  
  # Convert back to interval (0, period)
  cmean <- cmean + 2 * pi * revolution
  cmean <- cmean * period / (2 * pi)
  return(cmean)
  
}


#' Angular standard deviation
#' 
#' Circular data repeat every period which has to be taken into account
#' for calculating the standard deviation around the mean angle. 
#' 
#' @details
#' After transformation of the circular data to radians on the unit circle, the 
#' circular data can be expressed as complex complex vectors \eqn{z = e^{i
#' \theta\cdot n}}. If the circular data distribution is a wrapped normal or von
#' Mises distribution, the average value of \eqn{z} (the mean resultant) is
#' given by \eqn{\langle z\rangle = e^{i\mu - \frac{\sigma^2}{2}}}, with the
#' mean angle \eqn{\mu}. The length of the mean resultant is given by \eqn{|
#' \langle z\rangle| = e^{- \frac{\sigma^2}{2}} = \rho}. This yields an 
#' expression for the angular standard deviation \eqn{\sigma} around \eqn{\mu}:
#' \deqn{\sigma = \sqrt{\text{ln}(\rho^{-2})} = \sqrt{-2\text{ln}(\rho)}}
#' This is used to calculate the angular standard deviation. The value of 
#' \eqn{\rho} is given by the following expression:
#' \deqn{|\rho| = \sqrt{\bar{z}\bar{z^*}} = \sqrt{\left(\frac{1}{n}\sum_{i=1}^n
#' \text{sin}(\theta_i)\right)^2 + \left(\frac{1}{n}\sum_{i=1}^n\text{cos}
#' (\theta_i)\right)^2}}
#' @param x Numeric vector with circular data.
#' @param period A numeric value declaring the period of the circular data. The
#' default is `2 * pi`.
#' @return The angular standard deviation of the circular data as numeric value.
#' @family spread_measures
#' @export
angular_sd <- function(x, period = 2*pi) {
  
  # Check input types
  if (!all(is.numeric(x), is.numeric(period))) {
    stop("Invalid values! Both `x` and `period` must be numeric.")
  }
  
  # Convert to radians and count revolutions
  theta <- 2 * pi * x / period

  # Calculate angular standard deviation
  sx <- mean(sin(theta))
  cx <- mean(cos(theta))
  rho <- sqrt(sx*sx + cx*cx)
  sigma <- sqrt(-2*log(rho))
  
  # Convert back to interval (0, period)
  sigma <- sigma * period / (2 * pi)
  return(sigma)
  
}