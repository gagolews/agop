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
#' Transitive Binary Relations
#'
#' @description
#' A binary relation \eqn{R} is \emph{transitive}, iff
#' for all \eqn{x}, \eqn{y}, \eqn{z} we have \eqn{xRy} and \eqn{yRz}
#' \eqn{\Longrightarrow}{=>} \eqn{xRz}.
#'
#' @details
#' \code{rel_is_transitive} finds out if a given binary relation
#' is transitive. The algorithm has \eqn{O(n^3)} time complexity,
#' pessimistically, where
#' \eqn{n} is the number of rows in \code{R}.
#' If \code{R} contains missing values behind the diagonal,
#' the result will be \code{NA}.
#'
#' The \emph{transitive closure} of a binary relation \eqn{R},
#' determined by \code{rel_closure_transitive},
#' is the minimal superset of \eqn{R} such that it is transitive.
#' Here we use the well-known Warshall algorithm (1962),
#' which runs in \eqn{O(n^3)} time.
#'
#' The \emph{transitive reduction},
#' see (Aho et al. 1972), of an acyclic binary relation \eqn{R},
#' determined by \code{rel_reduction_transitive},
#' is a minimal unique subset \eqn{R'} of \eqn{R},
#' such that the transitive closures of \eqn{R} and \eqn{R'} are equal.
#' The algorithm implemented runs in \eqn{O(n^3)} time.
#' Note that a transitive reduction of a reflexive relation
#' is also reflexive. Moreover, some kind of transitive reduction
#' (not necessarily minimal) is also determined in
#' \code{\link{rel_reduction_hasse}} -- it is useful for
#' drawing Hasse diagrams.
#'
#'
#' @param R an object coercible to a 0-1 (logical) square matrix,
#' representing a binary relation on a finite set.
#'
#' @return The \code{rel_closure_transitive} and
#' \code{rel_reduction_transitive} functions
#' return a logical square matrix. \code{\link{dimnames}}
#' of \code{R} are preserved.
#'
#' On the other hand, \code{rel_is_transitive} returns
#' a single logical value.
#'
#' @references
#' Aho A.V., Garey M.R., Ullman J.D.,
#' The Transitive Reduction of a Directed Graph,
#' \emph{SIAM Journal on Computing} 1(2), 1972, pp. 131-137.
#'
#' Warshall S., A theorem on Boolean matrices,
#' \emph{Journal of the ACM} 9(1), 1962, pp. 11-12.
#'
#' @family binary_relations
#' @rdname rel_transitive
#' @export
rel_is_transitive <- function(R)
{
   .Call("rel_is_transitive", as.matrix(R), PACKAGE="agop") # args checked internally
}


#' @rdname rel_transitive
#' @export
rel_closure_transitive <- function(R)
{
   .Call("rel_closure_transitive", as.matrix(R), PACKAGE="agop") # args checked internally
}


#' @rdname rel_transitive
#' @export
rel_reduction_transitive <- function(R)
{
   .Call("rel_reduction_transitive", as.matrix(R), PACKAGE="agop") # args checked internally
}
