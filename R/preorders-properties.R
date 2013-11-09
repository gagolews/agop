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
#' Check if a Binary Relation is Reflexive
#' 
#' @description
#' A binary relation \eqn{R} is reflexive, iff
#' for all \eqn{x} we have \eqn{xRx}.
#' 
#' @details
#' The function just checks whether all elements
#' on the diagonal of \code{R} are non-zeros,
#' i.e. it has \eqn{O(n)} time complexity,
#' where \eqn{n} is the number of rows in \code{R}.
#' 
#' Any missing value on the diagonal outputs \code{NA}.
#' 
#' @param R an object coercible to a 0-1 (logical) square matrix,
#' representing a binary relation
#' 
#' @return Returns a single logical value.
#' @family binary_relations
#' @export
rel_is_reflexive <- function(R)
{
   .Call("rel_is_reflexive", as.matrix(R), PACKAGE="agop") # args checked internally 
}


#' @title
#' Check if a Binary Relation is Total
#' 
#' @description
#' A binary relation \eqn{R} is total, iff
#' for all \eqn{x}, \eqn{y} we have \eqn{xRy} or \eqn{yRx}.
#' 
#' @details
#' Note that a total relation is necessarily reflexive.
#' 
#' The algorithm has \eqn{O(n^2)} time complexity,
#' where \eqn{n} is the number of rows in \code{R}.
#' 
#' If \code{R[i,j]} and \code{R[j,i]} is \code{NA}
#' for some \eqn{(i,j)}, then the functions outputs \code{NA}.
#' 
#' @param R an object coercible to a 0-1 (logical) square matrix,
#' representing a binary relation
#' 
#' @return single logical value
#' @family binary_relations
#' @export
rel_is_total <- function(R)
{
   .Call("rel_is_total", as.matrix(R), PACKAGE="agop") # args checked internally 
}



#' @title
#' Check if a Binary Relation is Transitive
#' 
#' @description
#' A binary relation \eqn{R} is transitive, iff
#' for all \eqn{x}, \eqn{y}, \eqn{z} we have \eqn{xRy} and \eqn{yRz}
#' \eqn{\Longrightarrow}{=>} \eqn{xRz}.
#' 
#' @details
#' The algorithm has \eqn{O(n^3)} time complexity, pessimistically.
#' 
#' If \code{R} contains missing values behind the diagonal,
#' the result will be \code{NA}.
#' 
#' 
#' @param R an object coercible to a 0-1 (logical) square matrix,
#' representing a binary relation
#' @return Returns a single logical value.
#' @family binary_relations
#' @export
rel_is_transitive <- function(R)
{
   .Call("rel_is_transitive", as.matrix(R), PACKAGE="agop") # args checked internally 
#    # version 0.2
#    if (!is(B, 'igraph')) B <- graph.adjacency(B)
#    n <- vcount(B)
#    for (i in 1:n) { # for each vertex
#       # do breadth-first search from each vertex
#       # transitivity holds iff each reachable vertex is within distance of 1
#       disti <- graph.bfs(B, root=i, unreachable=FALSE, order=FALSE, dist=TRUE)$dist
#       disti <- disti[!is.nan(disti)]
#       if (length(disti) > 0 && any(disti > 1))
#          return(FALSE)
#    }
#    TRUE # reached here -> no FALSE -> is transitive :-)
}



#' @title
#' Transitive Closure of a Binary Relation
#' 
#' @description
#' A transitive closure of \eqn{R} is the minimal
#' superset of \eqn{R} such that it is transitive.
#' 
#' @details
#' Here we use the Warshall algorithm (1962),
#' which runs in \eqn{O(n^3)} time, where
#' \eqn{n} is the number of rows in \code{R}.
#' 
#' The function preserves \code{\link{dimnames}} of \code{R}.
#' Missing values are not allowed and result in an error.
#' 
#' @param R an object coercible to a 0-1 (logical) square matrix,
#' representing a binary relation
#' @return Returns a logical square matrix.
#' @export
#' @family binary_relations
rel_closure_transitive <- function(R)
{
   R <- as.matrix(R)
   Rprim <- .Call("rel_closure_transitive", as.matrix(R), PACKAGE="agop") # args checked internally
   dimnames(Rprim) <- dimnames(R)
   Rprim
}



#' @title
#' Transitive Reduction of a Binary Relation 
#' 
#' @description
#' Useful for draving Hasse diagrams.
#' 
#' Transitive reduction is defined as a  minimal relation having
#' the same transitive closure as R.
#' 
#' @param R an object coercible to a 0-1 (logical) square matrix,
#' representing a binary relation
#' @return object of class \code{Matrix}
#' @family binary_relations
#' @export
de_transitive <- function(B)
{
   if (is(B, 'igraph')) B <- get.adjacency(B)
   stopifnot(is.matrix(B) || is(B, 'Matrix'), nrow(B) == ncol(B), nrow(B) > 0)
   
   # slow as hell!
   n <- nrow(B)
   Matrix::diag(B) <- 0
   for (i in 1:n) {
      for (j in 1:n) {
         for (k in 1:n) {
            if (as.logical(B[i,j]) &&  as.logical(B[j,k]) && as.logical(B[i,k])
               && !as.logical(B[k,i]) && !as.logical(B[k,j]) && !as.logical(B[j,i]))
               B[i,k] <- 0
         }
      }
   }
   B
}




#' @title
#' Total Closure of a Binary Relation [Fair Totalization]
#' 
#' @description
#' Reflexive closure of \eqn{R} is a minimal reflexive
#' superset \eqn{R'} of \eqn{R}.
#' 
#' @details
#' The function preserves \code{\link{dimnames}} of \code{R}.
#' 
#' @param R an object coercible to a 0-1 (logical) square matrix,
#' representing a binary relation
#' 
#' @return Returns a logical square matrix.
#' @export
#' @family binary_relations
rel_closure_reflexive <- function(R)
{
   R <- as.matrix(R)
   Rprim <- .Call("rel_closure_reflexive", as.matrix(R), PACKAGE="agop") # args checked internally
   dimnames(Rprim) <- dimnames(R)
   Rprim
}



#' @title
#' Total Closure of a Binary Relation [Fair Totalization]
#' 
#' @description
#' Fair totalization of \eqn{R} is a superset \eqn{R'} of \eqn{R}
#' such that if not \eqn{xRy} and not \eqn{yRx}
#' then \eqn{xR'y} and \eqn{yRx}.
#' 
#' @details
#' Note that a total binary relation is always reflexive.
#' 
#' Even if \eqn{R} is transitive, the resulting relation
#' may not necessarily fulfill this property.
#' If you want a total preorder,
#' call \code{\link{rel_closure_transitive}} afterwards.
#' 
#' Missing values are not allowed and result in an error.
#' 
#' @param R an object coercible to a 0-1 (logical) square matrix,
#' representing a binary relation
#' 
#' @return Returns a logical square matrix.
#' @export
#' @family binary_relations
#' 
#' @references
#' Gagolewski M., Scientific Impact Assessment Cannot be Fair,
#'    Journal of Informetrics 7(4), 2013, pp. 792-802.\cr
rel_closure_total_fair <- function(R)
{
   R <- as.matrix(R)
   Rprim <- .Call("rel_closure_total_fair", as.matrix(R), PACKAGE="agop") # args checked internally
   dimnames(Rprim) <- dimnames(R)
   Rprim
}

