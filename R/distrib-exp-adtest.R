## This file is part of the 'agop' library.
##
## Copyright 2013 Marek Gagolewski, Anna Cena
##
## Parts of the code are taken from the 'CITAN' R package by Marek Gagolewski
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


#' @title Anderson-Darling Test for Exponentiality
#' 
#' @description
#' Performs an approximate Anderson-Darling goodness-of-fit
#' test, which verifies the null hypothesis:
#' Data follows an exponential distribution.
#'
#' @details
#' Sample size should not be smaller than 3. Missing values
#' are removed from \code{x} before applying the procedure.
#' 
#' The p-value is approximate.... DETAILS....
#' 
#' .....TO DO.....
#'
#' 
#' @param x a non-negative numeric vector of data values
#' 
#' @return
#' A list of the class \code{htest} is returned,
#' just like in many other testing methods,
#' see e.g. \code{\link{ks.test}}.
#' 
#' @export
#' @seealso \code{\link{pexp}}
#' 
#' @family Tests
#' 
#' @references
#' Anderson T.W., Darling D.A.,
#' A Test of Goodness-of-Fit, Journal of the American Statistical Association 49, 
#' 1954, pp. 765-769.
exp_test_ad <- function(x)
{
   DNAME <- deparse(substitute(x))
   print(DNAME)
   stop("TO DO")
}

