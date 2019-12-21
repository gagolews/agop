## This file is part of the 'agop' library.
##
## Copyright 2013-2019 Marek Gagolewski, Anna Cena
##
## 'agop' is free software: you can redistribute it and/or modify
## it under the terms of the GNU Lesser General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## 'agop' is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
## GNU Lesser General Public License for more details.
##
## You should have received a copy of the GNU Lesser General Public License
## along with 'agop'. If not, see <http://www.gnu.org/licenses/>.


#' @title Anderson-Darling Test for the Pareto Type-II Distribution
#'
#' @description
#' Performs an approximate Anderson-Darling goodness-of-fit
#' test, which verifies the null hypothesis:
#' Data follow a Pareto-Type II distribution.
#'
#' @details
#' We know that if \eqn{X} follows a Pareto-Type II distribution
#' with shape parameter \eqn{k}, then \eqn{log(1+X/s)} follows an
#' exponential distribution with parameter \eqn{k}.
#' Thus, this function transforms the input vector,
#' and performs the same steps as \code{\link{exp_test_ad}}.
#'
#' @param x a non-negative numeric vector of data values
#' @param s the known scale parameter, \eqn{s>0}
#'
#' @return
#' A list of the class \code{htest} is returned,
#' see \code{\link{exp_test_ad}}.
#'
#' @export
#' @family Pareto2
#' @family Tests
pareto2_test_ad <- function(x, s=1)
{
   DNAME <- deparse(substitute(x))

   x <- x[!is.na(x)]
   n <- length(x)

   x <- log(1+x/s)

   n2 <- if(n < nrow(exp_test_ad_cdf)) n else nrow(exp_test_ad_cdf)
   if (any(is.na(exp_test_ad_cdf[n2,])))
      stop("Sample size too small")

   W <- .Call("exp_test_statistic", x, PACKAGE="agop")
   pv <- if (W > exp_test_ad_cdf[1, ncol(exp_test_ad_cdf)]) 1e-16 else
      1-splinefun(exp_test_ad_cdf[1,], exp_test_ad_cdf[n2,], method="monoH.FC")(W)


   res <- list(statistic=c(W=W), p.value=pv,
               method="Anderson-Darling goodness-of-fit test for Pareto Type-II distribution",
               data.name=DNAME)
   attr(res, "class") <- "htest"
   res
}
