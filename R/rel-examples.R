## This file is part of the 'agop' library.
##
## Copyright 2013 Marek Gagolewski, Anna Cena
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
#' Weak Dominance Relation (Preorder)
#' 
#' @description
#' Checks whether a given numeric vector is (weakly) dominated
#' by another vector, in terms of (sorted) elements' values
#' and their count.
#' 
#' @details
#' We say that a numeric vector \code{x}
#' is weakly dominated by \code{y}, written as \eqn{x <= y},
#'  iff \eqn{nx <= ny} AND
#' for all \eqn{i = 1,...,n} \eqn{x_{(n-i+1)} <= y_{(m-i+1)}},
#' where \eqn{nx = length(x)} and \eqn{ny = length(y)}.
#' 
#' This dominance relation is symmetric, i.e. for all permutations
#' of input vectors' elements it gives the same value.
#' Such preorder is tightly related to impact functions:
#' each impact function is a morphism between
#' wead-dominance-preordered set of vectors
#' and the set of reals equipped with standard linear ordering
#' (see Gagolewski, Grzegorzewski, 2011).
#' 
#' 
#' This function only accepts vectors with nonnegative elements.
#' 
#' @param x numeric vector
#' @param y numeric vector
#' 
#' @return single logical value; whether \code{x} is weakly
#' dominated by \code{y}
#' 
#' @references
#' Gagolewski M., Grzegorzewski P., Possibilistic Analysis of Arity-Monotonic 
#' Aggregation Operators and Its Relation to Bibliometric Impact Assessment of Individuals, 
#' International Journal of Approximate Reasoning 52(9), 2011, pp. 1312-1324.\cr
#' Gagolewski M., Scientific Impact Assessment Cannot be Fair,
#'  Journal of Informetrics 7(4), 2013, pp. 792-802.
#' 
#' @family binary_relations
#' @export
pord_weakdom <- function(x, y)
{
   .Call("pord_weakdom", x, y, PACKAGE="agop")
}


#' @title
#' Compare Vectors' Spread (Preorder)
#' 
#' @description
#' The \code{pord_spread} function determines
#' one numeric vector has not greater spread than the
#' other. Moreover, \code{pord_spreadsym} implements
#' the symmetrized spread relation.
#' 
#' @details
#' We say that \code{x} of size \eqn{n}
#' has no greater spread than \code{y}
#' iff for all \eqn{i,j=1,\dots,n} such
#' that \eqn{x_i>x_j} it holds \eqn{x_i-x_j\le y_i-y_j}.
#' Such preorder is used in the definition of
#' dispersion operators (see Gagolewski, 2013).
#' 
#' These functions accept only vectors of the same size.
#' 
#' @param x numeric vector
#' @param y numeric vector of the same length as \code{x}
#' 
#' @return Both functions return a single logical value,
#' which states whether \code{x} has no greater
#' spread than \code{y}
#' 
#' @references
#' Gagolewski M., Dispersion Operators, submitted paper, 2013.
#' 
#' @family binary_relations
#' @rdname pord_spread
#' @export
pord_spread <- function(x, y)
{
   .Call("pord_spread", x, y, PACKAGE="agop")
}


#' @rdname pord_spread
#' @export
pord_spreadsym <- function(x, y)
{
   .Call("pord_spreadsym", x, y, PACKAGE="agop")
}


#' @title
#' Create an Adjacency Matrix Representing a Binary Relation
#' 
#' @description
#' Returns a binary relation that represents results
#' of comparisons with \code{pord}
#' of all pairs of elements in \code{x}.
#' We have \code{ret[i,j] == pord(x[[i]], x[[j]], ...)}.
#' 
#' @param x list with elements to compare, preferrably named
#' @param pord a function with two arguments, returning a single boolean value,
#' e.g. \code{\link{pord_spread}}, \code{\link{pord_spreadsym}},
#' or \code{\link{pord_weakdom}}
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
   ord <- matrix(FALSE, nrow=n, ncol=n)
   colnames(ord) <- names(x)
   rownames(ord) <- names(x)
   
   for (i in seq_along(x)) {
      for (j in seq_along(x)) {
         if (pord(x[[i]], x[[j]], ...))
            ord[i,j] <- TRUE
      }
   }
   
   ord
}

