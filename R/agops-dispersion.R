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



#' @title
#' D2OWA Operators
#'
#' @description
#' Computes the D2OWA operator, i.e.,
#' the normalized L2 distance between a numeric vector and an OWA operator.
#'
#' @details
#' D2OWA is a symmetric dispersion function
#' It is defined as
#' \code{d2owa(x) == sqrt(mean((x-owa(x,w))^2))}.
#' Not all weights, however, generate a proper function of this kind;
#' \code{d2owa_checkwts} may be used to check that.
#' For \code{d2owa}, if \code{w} is not proper, an error is thrown.
#'
#' \code{w} is automatically normalized so that its elements sums up to 1.
#'
#' @param x numeric vector to be aggregated
#' @param w numeric vector of the same length as \code{x}, with elements in \eqn{[0,1]},
#' and such that \eqn{\sum_i w_i=1}{sum(x)=1}; weights
#' @return For \code{d2owa}, a single numeric value is returned.
#' On the other hand, \code{d2owa_checkwts} returns a single logical value.
#'
#' @rdname d2owa
#' @export
#' @family dispersion_functions
#' @references
#' Gagolewski M., Spread measures and their relation to aggregation functions,
#'     European Journal of Operational Research 241(2), 2015, pp. 469-477.
#'     doi:10.1016/j.ejor.2014.08.034
#'
#' Gagolewski M., Data Fusion: Theory, Methods, and Applications,
#'    Institute of Computer Science, Polish Academy of Sciences, 2015, 290 pp.
#'    isbn:978-83-63159-20-7
#'
#' Yager R.R., On ordered weighted averaging aggregation operators
#' in multicriteria decision making, \emph{IEEE Transactions on Systems,
#' Man, and Cybernetics} 18(1), 1988, pp. 183-190.
d2owa_checkwts <- function(w) {
   .Call("d2owa_checkwts", w, PACKAGE="agop")
}


#' @rdname d2owa
#' @export
d2owa <- function(x, w=rep(1/length(x), length(x))) {
   if (!d2owa_checkwts(w))
      stop('given weighting vector does not generate a dispersion function')
   sqrt(mean((x-owa(x,w))^2))
}
