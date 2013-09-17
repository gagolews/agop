## This file is part of the 'agop' library.
##
## Copyright 2013 Marek Gagolewski, Anna Cena
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
#' Data follows a Pareto-Type II distribution.
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
   stopifnot(is.numeric(s), length(s) == 1, is.finite(s), s > 0)
   stop("TO DO")
}

