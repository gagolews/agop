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
#' Cyclic Binary Relations
#'
#' @description
#' A binary relation \eqn{R} is \emph{cyclic}, iff
#' its transitive closure is not antisymmetric.
#' Note that \eqn{R} may be reflexive and still acyclic,
#' i.e., loops in \eqn{R} are not taken into account.
#'
#' @details
#' \code{rel_is_cyclic} has \eqn{O(n^3)} time complexity,
#' where \eqn{n} is the number of rows in \code{R}
#' (the implemented algorithm currently verifies whether a depth-first search-based
#' topological sorting is possible).
#' Missing values in \code{R} always result in \code{NA}.
#'
#' @param R an object coercible to a 0-1 (logical) square matrix,
#' representing a binary relation on a finite set.
#'
#' @return \code{rel_is_cyclic} returns
#' a single logical value.
#'
#' @export
#' @family binary_relations
#' @rdname rel_cyclic
rel_is_cyclic <- function(R)
{
   .Call("rel_is_cyclic", as.matrix(R), PACKAGE="agop") # args checked internally
}
