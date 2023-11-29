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


#' @title
#' Discretized Pareto Type-II (Lomax) Distribution [TO DO]
#'
#' @description
#' Probability mass function, cumulative distribution function,
#' quantile function, and random generation for the
#' Discretized Pareto Type-II distribution with shape
#' parameter \eqn{k>0} and scale parameter \eqn{s>0}.
#'
#' [TO DO: rewrite in C, add NA handling, add working qdpareto2()]
#'
#' @details
#' If \eqn{X\sim\mathrm{DP2}(k,s)}{X~DP2(k,s)},
#' then \eqn{\lfloor Y\rfloor=X}{floor(Y)=X},
#' where \eqn{Y} has ordinary Pareto Type-II
#' distribution, see \code{\link{ppareto2}}.
#'
#' @param x,q vector of quantiles
#' @param p vector of probabilities
#' @param n integer; number of observations
#' @param k vector of shape parameters, \eqn{k>0}
#' @param s vector of scale parameters, \eqn{s>0}
#' @param lower.tail logical; if \code{TRUE} (default),
#' probabilities are \eqn{P(X \le x)}, and \eqn{P(X > x)} otherwise
#' @return
#' numeric vector;
#' \code{ddpareto2} gives the probability mass function,
#' \code{pdpareto2} gives the cumulative distribution function,
#' \code{qdpareto2} calculates the quantile function,
#' and \code{rdpareto2} generates random deviates.
#'
#' @export
#' @rdname DiscretizedPareto2
#' @family distributions
#' @family DiscretizedPareto2
rdpareto2 <- function(n, k=1, s=1)
{
   stopifnot(is.numeric(k), k > 0)
   stopifnot(is.numeric(s), s > 0)
   # n checked by runif
   floor(s * ((runif(n)^(-1.0/k)) - 1.0))
}



#' @export
#' @rdname DiscretizedPareto2
pdpareto2 <- function(q, k=1, s=1, lower.tail=TRUE)
{
   stopifnot(is.numeric(k), k > 0)
   stopifnot(is.numeric(s), s > 0)
   stopifnot(is.numeric(q))
   ret <- numeric(length(q)) # inits with zeros
   wp <- (q>=0)
   ret[wp] <- 1.0-(s/(s+floor(q[wp]+1.0)))^k
   if (identical(lower.tail, FALSE))
      1-ret
   else
      ret
}



#' @export
#' @rdname DiscretizedPareto2
qdpareto2 <- function(p, k=1, s=1, lower.tail=TRUE)
{
   stopifnot(is.numeric(k), k > 0)
   stopifnot(is.numeric(s), s > 0)
   stopifnot(is.numeric(p))
   if (identical(lower.tail, FALSE))
      p <- 1.0-p
   stop("TO DO")
#    ret <- numeric(length(p)) # inits with zeros
#    wp <- (p>=0.0 & p<=1.0)
#    ret[wp] <- s*((1.0-p[wp])^(-1/k)-1.0)
#    ints <- (abs(ret[wp]-round(ret[wp])) < sqrt(.Machine$double.eps))
#    ret[wp][ints] <- round(ret[wp][ints])
#    ret[wp] <- floor(ret[wp])-1
#    ret[!wp] <- NaN
#    ret
}



#' @export
#' @rdname DiscretizedPareto2
ddpareto2 <- function(x, k=1, s=1)
{
   stopifnot(is.numeric(k), k > 0)
   stopifnot(is.numeric(s), s > 0)
   stopifnot(is.numeric(x))
   ret <- numeric(length(x)) # inits with zeros
   wp <- (x == floor(x) & x>=0)
   ret[wp] <- (s/(s+x[wp]))^k-(s/(s+x[wp]+1.0))^k
   ret
}
