#' Estimate smoothing parameter of circular density distribution (Taylor 2008)
#'
#' This function estimates the optimal smoothing parameter (bandwidth) of a 
#' circular density distribution according to the von Mises-scale plug-in rule 
#' introduced by Taylor 2008.
#' 
#' For the von Mises density function \eqn{\hat{f}(\theta|\nu) = \frac{1}{2\pi 
#' n I_0(\nu)}\sum_{i=1}^n\text{exp}\left(\nu\cos(\theta - \theta_i)\right)
#' \sim\text{vM}(\mu,\kappa)}, the smoothing parameter can be estimated with the
#' following expression after asymptotic approximation of the mean integrated 
#' squared error:
#' 
#' \deqn{\nu = \left(\frac{3n\hat{\kappa}^2 I_2(2\hat{\kappa})}{4\sqrt{\pi}I_0
#' (\hat{\kappa})^2}\right)^{\frac{2}{5}}}
#' 
#' @param x numeric vector with cyclical data
#' @param kappa.est numeric value with an estimate for von Mises concentration
#' parameter \eqn{\hat{\kappa}}. If `kappa.est = NULL` (the default), the 
#' parameter is estimated based on `x`.
#' @return numeric value with the estimated smoothing parameter (bandwidth) 
#' \eqn{\nu}.
#' @export
bw_taylor <- function(x, kappa.est = NULL){
  x <- circular::as.circular(
    x = x,
    type = "angles",
    units = "radians",
    modulo = "2pi",
    zero = 0,
    rotation = "clock",
    template = "none"
  )
  n <- length(x)
  if(is.null(kappa.est)){
    kappa.est <- MlevonmisesRad(x, mu=NULL, kappa=NULL, bias=TRUE)[4]
  }
  i0 <- besselI(x = kappa.est, nu = 0, expon.scaled = FALSE)
  i2 <- besselI(x = 2 * kappa.est, nu = 2, expon.scaled = FALSE)
  bw <- ((3 * n * kappa.est^2 * i2) / (4 * sqrt(pi) * i0^2))^0.4
  return(bw)
}
