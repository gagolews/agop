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
#' Total Binary Relations
#'
#' @description
#' A binary relation \eqn{R} is \emph{total}
#' (or \emph{strong complete}), iff
#' for all \eqn{x}, \eqn{y} we have \eqn{xRy} or \eqn{yRx}.
#'
#' @details
#' Note that each total relation is also reflexive,
#' see \code{\link{rel_is_reflexive}}.
#'
#' \code{rel_is_total} determines if a given binary relation
#' \code{R} is total.
#' The algorithm has \eqn{O(n^2)} time complexity,
#' where \eqn{n} is the number of rows in \code{R}.
#' If \code{R[i,j]} and \code{R[j,i]} is \code{NA}
#' for some \eqn{(i,j)}, then the functions outputs \code{NA}.
#'
#' The problem of finding a total closure or reduction
#' is not well-defined in general.
#'
#' When dealing with preorders, however, the following
#' closure may be useful, see (Gagolewski, 2013).
#' \emph{Fair totalization} of \eqn{R}, performed by
#' \code{rel_closure_total_fair}, is the minimal superset \eqn{R'} of \eqn{R}
#' such that if not \eqn{xRy} and not \eqn{yRx}
#' then \eqn{xR'y} and \eqn{yR'x}.
#'
#' Even if \code{R} is transitive, the resulting relation
#' might not necessarily fulfil this property.
#' If you want a total preorder,
#' call \code{\link{rel_closure_transitive}} afterwards.
#' Missing values in \code{R} are not allowed and result in an error.
#'
#' @param R an object coercible to a 0-1 (logical) square matrix,
#' representing a binary relation on a finite set.
#'
#' @references
#' Gagolewski M., Scientific Impact Assessment Cannot be Fair,
#'    \emph{Journal of Informetrics} 7(4), 2013, pp. 792-802.\cr
#'
#' Gagolewski M., Data Fusion: Theory, Methods, and Applications,
#'    Institute of Computer Science, Polish Academy of Sciences, 2015, 290 pp.
#'    isbn:978-83-63159-20-7
#'
#' @return \code{rel_is_total} returns a single logical value.
#'
#' \code{rel_closure_reflexive} returns a logical square matrix.
#' \code{\link{dimnames}} of \code{R} are preserved.
#'
#' @family binary_relations
#' @export
#' @rdname rel_total
rel_is_total <- function(R)
{
   .Call("rel_is_total", as.matrix(R), PACKAGE="agop") # args checked internally
}


#' @export
#' @rdname rel_total
rel_closure_total_fair <- function(R)
{
   .Call("rel_closure_total_fair", as.matrix(R), PACKAGE="agop") # args checked internally
}
