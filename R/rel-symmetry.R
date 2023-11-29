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
#' Symmetric Binary Relations
#'
#' @description
#' A binary relation \eqn{R} is \emph{symmetric}, iff
#' for all \eqn{x, y} we have \eqn{xRy} \eqn{\Rightarrow}{=>} \eqn{yRx}.
#'
#' @details
#' \code{rel_is_symmetric} finds out if a given binary relation
#' is symmetric. Any missing value behind the diagonal results in \code{NA}.
#'
#' The \emph{symmetric closure} of a binary relation \eqn{R},
#' determined by \code{rel_closure_symmetric},
#' is the smallest symmetric binary relation that contains \eqn{R}.
#' Here, any missing values in \code{R} result in an error.
#'
#' @param R an object coercible to a 0-1 (logical) square matrix,
#' representing a binary relation on a finite set.
#'
#' @return The \code{rel_closure_symmetric} function
#' returns a logical square matrix. \code{\link{dimnames}}
#' of \code{R} are preserved.
#'
#' On the other hand, \code{rel_is_symmetric} returns
#' a single logical value.
#'
#' @export
#' @family binary_relations
#' @rdname rel_symmetric
rel_is_symmetric <- function(R)
{
   .Call("rel_is_symmetric", as.matrix(R), PACKAGE="agop") # args checked internally
}


#' @export
#' @rdname rel_symmetric
rel_closure_symmetric <- function(R)
{
   .Call("rel_closure_symmetric", as.matrix(R), PACKAGE="agop") # args checked internally
}
