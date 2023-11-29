## This file is part of the 'agop' library.
##
## Copyleft (c) 2013-2023, Marek Gagolewski <https://www.gagolewski.com/>
##
##
## 'agop' is free software: you can redistribute it and/or modify it under
## the terms of the GNU Lesser General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## 'agop' is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
## GNU Lesser General Public License for more details.
##
## A copy of the GNU Lesser General Public License can be downloaded
## from <http://www.gnu.org/licenses/>.



#' @title Parameter Estimation in the Discretized Pareto-Type II Distribution Family (MLE)
#'
#' @description
#' Finds the maximum likelihood estimator of the Discretized Pareto Type-II distribution's
#' shape parameter \eqn{k} and scale parameter \eqn{s}.
#'
#' @details
#' Note that the maximum of the likelihood function might not exist
#' for some input vectors. This estimator may have a large mean squared error.
#'
#' @param x a non-negative numeric vector
#' @param kmin,kmax lower and upper bound for the shape parameter
#' @param smin,smax lower and upper bound for the scale parameter
#' @param k0,s0 initial points for the L-BFGS-B method
#' @return
#' Returns a numeric vector  with the following named components:
#' \itemize{
#' \item \code{k} - estimated parameter of shape
#' \item \code{s} - estimated parameter of scale
#' }
#' or \code{c(NA, NA)} if the maximum of the likelihood function
#' could not be found.
#' @export
#' @family DiscretizedPareto2
dpareto2_estimate_mle <- function(x, k0=1, s0=1, kmin=1e-4, smin=1e-4, kmax=100, smax=100)
{
   stopifnot(is.numeric(x), x>=0)
   like <- function(par, x) {
      -sum(log(ddpareto2(x, par[1], par[2])))
   }
   res<-optim(c(k0, s0), like, x=x, method="L-BFGS-B",
      lower=c(kmin, smin), upper=c(kmax, smax))

   if (res$convergence!=0)
      c(k=NA_real_, s=NA_real_)
   else
      c(k=res$par[1], s=res$par[2])
}
