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

   for (i in seq_along(x))
      for (j in seq_along(x))
         if (pord(x[[i]], x[[j]], ...))
            ord[i,j] <- TRUE

   ord
}



#' @title
#' Get Incomparable Pairs in a Binary Relation
#'
#' @description
#' Given a binary relation \eqn{R},
#' we would say that a pair \eqn{(x,y)} is incomparable iff
#' not \eqn{xRy} and not \eqn{yRx}.
#'
#' @details
#' See also \code{\link{get_independent_sets}} of how to generate
#' all maximal independent sets.
#'
#' @param B object of class \code{igraph} or a square
#' 0-1 matrix of class \code{Matrix} or \code{matrix}
#' @return Returns an integer matrix with two columns,
#' giving the indices of incomparable elements;
#' note that these are pairs, and not sets: you'll get \code{(i,j)} and \code{(j,i)}.
#' @export
#' @family binary_relations
get_incomparable_pairs <- function(B)
{
   if (is(B, 'igraph')) B <- get.adjacency(B)
   stopifnot(is.matrix(B) || is(B, 'Matrix'), nrow(B) == ncol(B), nrow(B) > 0)

   B2 <- B + Matrix::t(B)
   # 0s lie symmetricly over diagonal and indicate incomparable pairs
   Matrix::which(B2 == 0, arr.ind=TRUE)
}


#' @title
#' Get All Maximal Independent Sets in a Binary Relation
#'
#' @description
#' The function generates vectors of indices \eqn{S_j=\{i_1,...,i_{k_j}\}}
#' such that all pairs from \eqn{S_j} are incomparable
#' (A pair (i,i') is incomparable iff
#' not \eqn{i R i'} and not \eqn{i' R i},
#' see also \code{\link{get_incomparable_pairs}}.
#'
#'
#' @details
#' Note that we assume that \eqn{B} is transitive.
#' Loops are not taken into account at all.
#'
#' @param B object of class \code{igraph} or a square
#' 0-1 matrix of class \code{Matrix} or \code{matrix}
#' @return list of integer vectors; each list element defines
#' an independent set of vertices numbers
#'
#' @export
#' @family binary_relations
get_independent_sets <- function(B)
{
   if (!is(B, 'igraph')) B <- graph.adjacency(B)

   # create graph with all not-directly-connected nodes
   C <- (shortest.paths(B, mode="all") > 1) # symmetric
   diag(C) <- 0

   # find all maximal cliques (== independent sets)
   out <- maximal.cliques(graph.adjacency(C, mode="undirected"))
   out[sapply(out, length) > 1] # remove singletons
}



#' @title
#' Get All Equivalence Classes of a Total Binary Relation
#'
#'
#' @description
#' Note that we assume that \eqn{B} is total, reflexive and transitive.
#'
#' @param B object of class \code{igraph} or a square
#' 0-1 matrix of class \code{Matrix} or \code{matrix}
#' @return list of integer vectors; each list element defines
#' an equivalence class by listing vertices' numbers;
#' each vector is ordered by the outdegrees of their nodes (they are
#' the same in each class)
#'
#' @export
#' @family binary_relations
get_equivalence_classes <- function(B)
{
   if (!is(B, 'igraph')) { BAD <- as.matrix(B); B <- graph.adjacency(B) }
   else { BAD <- as.matrix(get.adjacency(B)) }
   stopifnot(rel_is_total(BAD), rel_is_transitive(BAD))

   # now B - igraph
   # now BAD - matrix

   n <- nrow(BAD)

   # create graph with all `symmetric` pairs
   C <- BAD & t(BAD) # logical AND
   out <- maximal.cliques(graph.adjacency(C, mode="undirected"))

   # now sort according to the out-degree (decreasingly)
   # every vertex in an equivalence class has the same degree
   # if x<y, then outdegree(x)>outdegree(y)
   extractFirst <- sapply(out, function(e) e[1])
   ord <- order(rowSums(BAD)[extractFirst], decreasing=TRUE)
   out[ord]
}
