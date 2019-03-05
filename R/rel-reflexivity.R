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
#' Reflexive Binary Relations
#'
#' @description
#' A binary relation \eqn{R} is reflexive, iff
#' for all \eqn{x} we have \eqn{xRx}.
#'
#' @details
#' \code{rel_is_reflexive} finds out if a given binary relation
#' is reflexive. The function just checks whether all elements
#' on the diagonal of \code{R} are non-zeros,
#' i.e. it has \eqn{O(n)} time complexity,
#' where \eqn{n} is the number of rows in \code{R}.
#' Missing values on the diagonal may result in \code{NA}.
#'
#' Reflexive closure of a binary relation \eqn{R},
#' determined by \code{rel_closure_reflexive},
#' is the minimal reflexive superset \eqn{R'} of \eqn{R}.
#'
#' Reflexive reduction of a binary relation \eqn{R},
#' determined by \code{rel_reduction_reflexive},
#' is the minimal subset \eqn{R'} of \eqn{R},
#' such that the reflexive closures of \eqn{R} and \eqn{R'} are equal
#' i.e., the largest irreflexive relation contained in \eqn{R}.
#'
#' @param R an object coercible to a 0-1 (logical) square matrix,
#' representing a binary relation on a finite set.
#'
#' @return The \code{rel_closure_reflexive} and
#' \code{rel_reduction_reflexive} functions
#' return a logical square matrix. \code{\link{dimnames}}
#' of \code{R} are preserved.
#'
#' On the other hand, \code{rel_is_reflexive} returns
#' a single logical value.
#'
#' @export
#' @family binary_relations
#' @rdname rel_reflexive
rel_is_reflexive <- function(R)
{
   .Call("rel_is_reflexive", as.matrix(R), PACKAGE="agop") # args checked internally
}

#' @export
#' @rdname rel_reflexive
rel_closure_reflexive <- function(R)
{
   .Call("rel_closure_reflexive", as.matrix(R), PACKAGE="agop") # args checked internally
}


#' @export
#' @rdname rel_reflexive
rel_reduction_reflexive <- function(R)
{
   .Call("rel_reduction_reflexive", as.matrix(R), PACKAGE="agop") # args checked internally
}
