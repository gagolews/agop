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
#' Check If Two Vectors Are Comonotonic
#'
#' @description
#' This functions determines if two vectors have a common
#' ordering permutation.
#'
#' @details
#' Two vectors \code{x}, \code{y} of equal length \eqn{n} are \emph{comonotonic},
#' if and only if there exists a permutation \eqn{\sigma} such that
#' \eqn{x_{\sigma(1)}\le \dots \le x_{\sigma(n)}} and
#' \eqn{y_{\sigma(1)}\le \dots \le y_{\sigma(n)}}.
#' Thus, \eqn{\sigma} orders \code{x} and \code{y} simultaneously.
#' Equivalently, \code{x} and \code{y} are comonotonic,
#' iff \eqn{(x_i-x_j)(y_i-y_j)\ge 0} for every \code{i,j}.
#'
#' If there are missing values in \code{x} or \code{y}, the function
#' returns \code{NA}.
#'
#' Currently, the implemented algorithm  has \eqn{O(n^2)} time complexity.
#'
#' @param x numeric vector
#' @param y numeric vector
#' @param incompatible_lengths single logical value,
#' value to return iff lengths of \code{x} and \code{y} differ
#'
#' @return
#' Returns a single logical value.
#'
#' @export
#' @family binary_relations
#' @references
#' Grabisch M., Marichal J.-L., Mesiar R., Pap E., \emph{Aggregation functions},
#'    Cambridge University Press, 2009.
#'
#' Gagolewski M., Data Fusion: Theory, Methods, and Applications,
#'    Institute of Computer Science, Polish Academy of Sciences, 2015, 290 pp.
#'    isbn:978-83-63159-20-7
check_comonotonicity <- function(x, y, incompatible_lengths=NA) {
   .Call("check_comonotonicity", x, y, incompatible_lengths, PACKAGE="agop")
}
