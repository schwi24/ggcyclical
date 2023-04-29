#' Directional speed
#' 
#' Some directional data in 2-dimensions include velocities (speed measures), 
#' e.g. wind directions or the intracellular movement of mitochondria. 
#' This package implements the following measures of directional speed. Many of
#' them are based on Yamartino 1984.
#' @family directional_speed
#' @name directional_speed
NULL


#' Speed-weighted angular mean (Yamartino 1984)
#' 
#' Angular data repeat every 2*pi radians which has to be taken into account
#' for calculating the angular means. For directional velocities, the speed 
#' values can be used to weight the mean using this function based on the 
#' speed-weighted angular mean (Yamartino 1984).
#' @details
#' The mean wind direction weighted by the wind speed is calculated by
#' \deqn{\theta_v = \text{atan}(V_{xa},V_{ya})}
#' with the average velocity in the x (\eqn{V_{xa} = \frac{1}{n}\sum_{i=1}^n V_i
#' \cdot\text{sin}(\theta_i)}) and y (\eqn{V_{ya} = \frac{1}{n}\sum_{i=1}^n V_i
#' \cdot\text{cos}(\theta_i)}) directions, respectively.
#' @param angle Numeric vector with angles
#' @param speed Numeric vector with velocities used to weight the mean
#' @param period A numeric value declaring the period of the circular data. The
#' default is `2 * pi`.
#' @return The average angle in radians or degrees.
#' @family directional_speed
#' @family location_measures
#' @export
weighted_angular_mean <- function(angle, speed, period = 2*pi){
  
  # Check input types
  if(!is.numeric(angle)){
    stop("Invalid type! `x` must be numeric.")
  }
  if(!is.numeric(speed)){
    stop("Invalid type! `speed` must be numeric.")
  }
  if(!is.numeric(period)){
    stop("Invalid type! `period` must be numeric.")
  }

  # Convert angles to radians
  angle <- 2 * pi * angle / period

  # Calculate weighted mean angle and map to unit circle
  sx <- mean(speed * sin(angle))
  cx <- mean(speed * cos(angle))
  mean_angle <- atan2(sx, cx)
  mean_angle <- nice_angles(
    x = mean_angle,
    period = 2 * pi,
    force_radians = TRUE
  )
  
  # Convert back to degree and return mean angle
  mean_angle <- period * mean_angle / (2 * pi)
  
  return(mean_angle)
  
}

#' Approximated angular standard deviation (Yamartino 1984)
#' 
#' Angular data repeat every 2*pi radians which has to be taken into account
#' for calculating the standard deviation over angles. This function uses the 
#' single-pass approximation from Yamartino 1984 to calculate the angular 
#' standard deviation.
#' @details
#' For precise calculation of the variance \eqn{\sigma_\theta^2}, first the mean
#' angle \eqn{\theta_a} has to be calculated followed by two additional mean 
#' values \eqn{{\sigma_\theta}^2 = \mu({\Delta_i}^2) - (\mu(\Delta_i))^2} in 
#' order to account for the continuous border at \eqn{(0,2\pi)}, with \eqn{
#' |\Delta_i| = \text{min}\left(|\theta_i-\theta_a|,2\pi - |\theta_i-\theta_a|
#' \right)}. Yamartino 1984 introduces therefore a single-pass approximation by 
#' interpolation between the edge cases of small angles (\eqn{\sigma_\theta
#' \approx\text{asin}(\varepsilon)}) and maximal variance (\eqn{{\sigma_\theta}
#' ^2\approx\frac{\pi^2}{3}}). This resulted in the following equation:
#' \deqn{\sigma_\theta\approx\text{asin}(\varepsilon)\left(1 + b\varepsilon^3
#' \right),}
#' with \eqn{b=\frac{2}{\sqrt{3}}-1} and \eqn{\varepsilon=\sqrt{1-\left(\mu(
#' \text{sin}(\theta_i))^2 + \mu(\text{cos}(\theta_i))^2\right)}}.
#' @param x Numeric vector
#' @param period A numeric value declaring the period of the circular data. The
#' default is `2 * pi`.
#' @return The average angle in radians or degrees.
#' @family directional_speed
#' @family spread_measures
#' @export
angular_sd_yamartino <- function(x, period = 2*pi){
  
  # Check input types
  if(!is.numeric(x)){
    stop("Invalid type! x must be numeric.")
  }
  if(!is.numeric(period)){
    stop("Invalid type! `period` must be numeric.")
  }

  # Convert angles to radians
  angle <- 2 * pi * x / period
  
  # Calculate mean angle
  sx <- mean(sin(angle))
  cx <- mean(cos(angle))
  eps <- sqrt(1-(sx*sx + cx*cx))
  sd_angle <- asin(eps) * (1 + eps*eps*eps*(2/sqrt(3)-1))
  
  # Convert back to degree and return mean angle
  sd_angle <-  period * sd_angle / (2 * pi)
  
  return(sd_angle)
  
}

