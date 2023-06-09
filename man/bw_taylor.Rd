% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bw-taylor.R
\name{bw_taylor}
\alias{bw_taylor}
\title{Estimate smoothing parameter of circular density distribution (Taylor 2008)}
\usage{
bw_taylor(x, kappa.est = NULL)
}
\arguments{
\item{x}{numeric vector with cyclical data}

\item{kappa.est}{numeric value with an estimate for von Mises concentration
parameter \eqn{\hat{\kappa}}. If \code{kappa.est = NULL} (the default), the
parameter is estimated based on \code{x}.}
}
\value{
numeric value with the estimated smoothing parameter (bandwidth)
\eqn{\nu}.
}
\description{
This function estimates the optimal smoothing parameter (bandwidth) of a
circular density distribution according to the von Mises-scale plug-in rule
introduced by Taylor 2008.
}
\details{
For the von Mises density function \eqn{\hat{f}(\theta|\nu) = \frac{1}{2\pi 
n I_0(\nu)}\sum_{i=1}^n\text{exp}\left(\nu\cos(\theta - \theta_i)\right)
\sim\text{vM}(\mu,\kappa)}, the smoothing parameter can be estimated with the
following expression after asymptotic approximation of the mean integrated
squared error:

\deqn{\nu = \left(\frac{3n\hat{\kappa}^2 I_2(2\hat{\kappa})}{4\sqrt{\pi}I_0
(\hat{\kappa})^2}\right)^{\frac{2}{5}}}
}
