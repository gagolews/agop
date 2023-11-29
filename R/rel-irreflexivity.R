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
#' Irreflexive Binary Relations
#'
#' @description
#' A binary relation \eqn{R} is \emph{irreflexive}
#' (or antireflexive), iff
#' for all \eqn{x} we have \eqn{\neg xRx}{!xRx}.
#'
#' @details
#' \code{rel_is_irreflexive} finds out if a given binary relation
#' is irreflexive. The function just checks whether all elements
#' on the diagonal of \code{R} are zeros,
#' i.e., it has \eqn{O(n)} time complexity,
#' where \eqn{n} is the number of rows in \code{R}.
#' Missing values on the diagonal may result in \code{NA}.
#'
#' When dealing with a graph's loops,
#' i.e., elements related to themselves, you may be interested
#' in finding a reflexive closure,
#' see \code{\link{rel_closure_reflexive}},
#' or a reflexive reduction,
#' see \code{rel_reduction_reflexive}.
#'
#' @param R an object coercible to a 0-1 (logical) square matrix,
#' representing a binary relation on a finite set.
#'
#' @return \code{rel_is_irreflexive} returns
#' a single logical value.
#'
#' @export
#' @family binary_relations
#' @rdname rel_irreflexive
rel_is_irreflexive <- function(R)
{
   .Call("rel_is_irreflexive", as.matrix(R), PACKAGE="agop") # args checked internally
}
