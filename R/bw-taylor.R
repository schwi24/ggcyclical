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
    kappa.est <- circular:::MlevonmisesRad(x, mu=NULL, kappa=NULL, bias=TRUE)[4]
  }
  i0 <- besselI(x = kappa.est, nu = 0, expon.scaled = FALSE)
  i2 <- besselI(x = 2 * kappa.est, nu = 2, expon.scaled = FALSE)
  bw <- ((3 * n * kappa.est^2 * i2) / (4 * sqrt(pi) * i0^2))^0.4
  return(bw)
}
