## This file is part of the 'agop' library.
##
## Copyright 2013 Marek Gagolewski, Anna Cena
##
## Parts of the code are taken from the 'CITAN' R package by Marek Gagolewski
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


#' Weak Dominance Relation (preorder)
#' 
#' \eqn{x <= y} iff \eqn{nx <= ny} AND
#' for all \eqn{i = 1,...,n} \eqn{x_{(n-i+1)} <= y_{(m-i+1)}},
#' where \eqn{nx = length(x)} and \eqn{ny = length(y)}.
#' 
#' @param x numeric vector
#' @param y numeric vector
#' @param disable_check logical value; if you are sure that
#' \code{x} and \code{y} are nonincreasingly sorted numeric vector,
#' set to \code{TRUE} to speed up the code execution
#' @return single logical value; whether x <= y
#' @export
pord_weakdom <- function(x, y, disable_check=FALSE)
{
   if (!identical(disable_check, TRUE)) {
      stopifnot(is.numeric(x), is.numeric(y))
      x <- sort(x, decreasing=TRUE)
      y <- sort(y, decreasing=TRUE)
   }
   length(x) <= length(y) && all(x <= y[1:length(x)])
}


#' Create Adjacency Matrix of Given Binary Relation
#' 
#' Note that adjacency matrix  can also be conceived as a directed graph (DAG).
#' 
#' @param x list with elements to compare, preferrably named
#' @param pord function with 2 arguments, returning boolean value
#' @param ... additional arguments passed to \code{pord}
#' @return object of class \code{Matrix}, with 1s and 0s
#' @export
rel_graph <- function(x, pord, ...)
{
   stopifnot(is.list(x))
   n <- length(x)
   ord <- Matrix(0L, nrow=n, ncol=n)
   colnames(ord) <- names(ex1)
   rownames(ord) <- names(ex1)
   
   for (i in seq_along(x)) {
      for (j in seq_along(x)) {
         if (pord(x[[i]], x[[j]]))
            ord[i,j] <- 1L
      }
   }
   
   ord
}


#' Check if Given Adjacency Matrix is Reflexive
#' 
#' A binary relation R is reflexive, iff
#' for all \code{x} we have \eqn{xRx}.
#' The function just checks whether all elements
#' on the diagonal of \code{B} are non-zeros.
#' 
#' @param B object of class \code{Matrix}, with 1s and 0s
#' @seealso \code{\link{rel_graph}}
#' @export
is_reflexive <- function(B)
{
   stopifnot(is(B, "Matrix"))
   all(diag(B) != 0)
}


#' Check if Given Adjacency Matrix is Total
#' 
#' A binary relation R is total, iff
#' for all \code{x}, \code{y} we have \eqn{xRy} or \eqn{yRx}.
#' 
#' @param B object of class \code{Matrix}, with 1s and 0s
#' @seealso \code{\link{rel_graph}}
#' @export
is_total <- function(B)
{
   stopifnot(is(B, "Matrix"))
   all(B + t(B) != 0)
}

