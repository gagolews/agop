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
#' Asymetric Binary Relations
#'
#' @description
#' A binary relation \eqn{R} is \emph{asymmetric}, iff
#' for all \eqn{x, y} we have \eqn{xRy} \eqn{\Rightarrow}{=>}
#' \eqn{\neg yRx}{!yRx}.
#'
#' @details
#' Note that an asymmetric relation is necessarily irreflexive,
#' compare \code{\link{rel_is_irreflexive}}.
#'
#' \code{rel_is_asymmetric} finds out if a given binary relation
#' is asymmetric. Missing values in \code{R} may result in \code{NA}.
#'
#' Also, check out \code{\link{rel_closure_symmetric}}
#' for the symmetric closure of \code{R}.
#'
#' @param R an object coercible to a 0-1 (logical) square matrix,
#' representing a binary relation on a finite set.
#'
#' @return \code{rel_is_asymmetric} returns
#' a single logical value.
#'
#' @export
#' @family binary_relations
#' @rdname rel_asymmetric
rel_is_asymmetric <- function(R)
{
   .Call("rel_is_asymmetric", as.matrix(R), PACKAGE="agop") # args checked internally
}
