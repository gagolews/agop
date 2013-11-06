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
#' Check if a Given Adjacency Matrix is Reflexive
#' 
#' @description
#' A binary relation \eqn{R} is reflexive, iff
#' for all \eqn{x} we have \eqn{xRx}.
#' The function just checks whether all elements
#' on the diagonal of \code{B} are non-zeros.
#' 
#' @param B object of class \code{igraph} or a square
#' 0-1 matrix of class \code{Matrix} or \code{matrix}.
#' 
#' @return Returns a single logical value.
#' @family binary_relations
#' @export
is_reflexive <- function(B)
{
   .Call("is_reflexive", as.matrix(B), PACKAGE="agop") # args checked internally 
   # stopifnot(is.matrix(B) || is(B, 'Matrix'), nrow(B) == ncol(B), nrow(B) > 0)
   #B <- as.logical(B)
   #all(Matrix::diag(B) != 0) # TO DO: add .Call
}


#' @title
#' Check if a Given Adjacency Matrix is Total
#' 
#' @description
#' A binary relation \eqn{R} is total, iff
#' for all \eqn{x}, \eqn{y} we have \eqn{xRy} or \eqn{yRx}.
#' 
#' A total relation is necessarily reflexive.
#' 
#' @param B object of class \code{igraph} or a square
#' 0-1 matrix of class \code{Matrix} or \code{matrix}
#' 
#' @return single logical value
#' @family binary_relations
#' @export
is_total <- function(B)
{
   .Call("is_total", as.matrix(B), PACKAGE="agop") # args checked internally 
   #if (is(B, 'igraph')) B <- get.adjacency(B)
   #stopifnot(is.matrix(B) || is(B, 'Matrix'), nrow(B) == ncol(B), nrow(B) > 0)
   #B <- as.logical(B)
   #all(B + Matrix::t(B) != 0) # TO DO: add .Call
}



#' @title
#' Check if a Given Adjacency Matrix is Transitive
#' 
#' @description
#' A binary relation \eqn{R} is transitive, iff
#' for all \eqn{x}, \eqn{y}, \eqn{z} we have \eqn{xRy} and \eqn{yRz}
#' \eqn{\Longrightarrow}{=>} \eqn{xRz}.
#' 
#' @param B object of class \code{igraph} or a square
#' 0-1 matrix of class \code{Matrix} or \code{matrix}
#' @return single logical value
#' @family binary_relations
#' @export
is_transitive <- function(B)
{
   .Call("is_transitive", as.matrix(B), PACKAGE="agop") # args checked internally 
#    # version 0.1
#    if (is(B, 'igraph')) B <- get.adjacency(B)
#    stopifnot(is.matrix(B) || is(B, 'Matrix'), nrow(B) == ncol(B), nrow(B) > 0)
#    # slow as hell!
#    # @TODO - make faster
#    n <- nrow(B)
#    for (i in 1:n) {
#       for (j in 1:n) {
#          for (k in 1:n) {
#             if (as.logical(B[i,j]) &&  as.logical(B[j,k]) && !as.logical(B[i,k]))
#                return(FALSE)
#          }
#       }
#    }
#    TRUE
   
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
   
   
   # version 0.3 - todo
   # use shortest.paths()
}


#' @title
#' Transitive Reduction of a Graph 
#' 
#' @description
#' Useful for draving Hasse diagrams.
#' 
#' Transitive reduction is defined as a  minimal relation having
#' the same transitive closure as R.
#' 
#' @param B object of class \code{igraph} or a square
#' 0-1 matrix of class \code{Matrix} or \code{matrix}
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
#' Transitive Closure of an Adjacency Matrix
#' 
#' @description
#' This may be slow for large graphs.
#' 
#' @param B object of class \code{igraph} or a square
#' 0-1 matrix of class \code{Matrix} or \code{matrix}
#' @return object of class \code{Matrix}
#' @export
#' @family binary_relations
closure_transitive <- function(B)
{
   if (is_transitive(B)) return(B) # do nothing
   if (is(B, 'igraph')) B <- get.adjacency(B)
   stopifnot(is.matrix(B) || is(B, 'Matrix'), nrow(B) == ncol(B), nrow(B) > 0)
   
   stop('resulting matrix is not necessarily transitive')
   
   # slow as hell!
   n <- nrow(B)
   for (i in 1:n) {
      for (j in 1:n) {
         for (k in 1:n) {
            if (as.logical(B[i,j]) &&  as.logical(B[j,k]) && !as.logical(B[i,k]))
               B[i,k] <- 1
         }
      }
   }
   B
}



#' @title
#' Total Closure of an Adjacency Matrix [Fair Totalization]
#' 
#' @description
#' Fair totalization: for each pair (x,y) s.t.
#' not xRy and not xRy let from now on xRy and yRx
#' 
#' @details
#' If you want a total preorder, call \code{\link{closure_transitive}}.
#' 
#' @param B object of class \code{igraph} or a square
#' 0-1 matrix of class \code{Matrix} or \code{matrix}
#' @return object of class \code{Matrix}
#' @export
#' @family binary_relations
closure_total_fair <- function(B)
{
   if (is(B, 'igraph')) B <- get.adjacency(B)
   stopifnot(is.matrix(B) || is(B, 'Matrix'), nrow(B) == ncol(B), nrow(B) > 0)
   
   B2 <- B + Matrix::t(B)
   # 0s lie symmetricly over diagonal and indicate incomparable pairs
   wh <- Matrix::which(B2 == 0, arr.ind=TRUE)
   B[wh] <- 1
   B
}


