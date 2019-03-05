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
#' Create an Adjacency Matrix Representing a Binary Relation
#'
#' @description
#' Returns a binary relation that represents results
#' of comparisons with \code{pord}
#' of all pairs of elements in \code{x}.
#' We have \code{ret[i,j] == pord(x[[i]], x[[j]], ...)}.
#'
#' @param x list with elements to compare, preferably named
#' @param pord a function with two arguments, returning a single boolean value,
#' e.g. \code{\link{pord_spread}},
#' \code{\link{pord_nd}}, or \code{\link{pord_weakdom}}
#' @param ... additional arguments passed to \code{pord}
#'
#' @return Returns a square logical matrix.
#' \code{\link{dimnames}} of the matrix correspond
#' to \code{\link{names}} of \code{x}.
#'
#' @family binary_relations
#' @export
rel_graph <- function(x, pord, ...)
{
   stopifnot(is.list(x))
   stopifnot(is.function(pord))
   n <- length(x)
   ord <- matrix(NA, nrow=n, ncol=n)
   colnames(ord) <- names(x)
   rownames(ord) <- names(x)

   for (i in seq_along(x))
      for (j in seq_along(x))
         ord[i,j] <- pord(x[[i]], x[[j]], ...)

   ord
}
