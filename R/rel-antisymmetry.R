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
#' Antisymmetric Binary Relations
#'
#' @description
#' A binary relation \eqn{R} is \emph{antisymmetric}, iff
#' for all \eqn{x, y} we have
#' \eqn{xRy} and \eqn{yRx} \eqn{\Rightarrow}{=>} \eqn{x=y}.
#'
#' @details
#' \code{rel_is_antisymmetric} finds out if a given binary relation
#' is antisymmetric. Missing values in \code{R} may result in \code{NA}.
#'
#' Also, check out \code{\link{rel_closure_symmetric}}
#' for the symmetric closure of \code{R}.
#'
#' @param R an object coercible to a 0-1 (logical) square matrix,
#' representing a binary relation on a finite set.
#'
#' @return \code{rel_is_antisymmetric} returns
#' a single logical value.
#'
#' @export
#' @family binary_relations
#' @rdname rel_antisymmetric
rel_is_antisymmetric <- function(R)
{
   .Call("rel_is_antisymmetric", as.matrix(R), PACKAGE="agop") # args checked internally
}
