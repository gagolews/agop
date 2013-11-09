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
#' Total Binary Relations
#' 
#' @description
#' A binary relation \eqn{R} is total, iff
#' for all \eqn{x}, \eqn{y} we have \eqn{xRy} or \eqn{yRx}.
#' 
#' @details
#' Note that each total relation is also reflexive,
#' see \code{\link{rel_is_reflexive}}.
#' 
#' \code{rel_is_total} determines if a given binary relation
#' \code{R} is total.
#' The algorithm has \eqn{O(n^2)} time complexity,
#' where \eqn{n} is the number of rows in \code{R}.
#' If \code{R[i,j]} and \code{R[j,i]} is \code{NA}
#' for some \eqn{(i,j)}, then the functions outputs \code{NA}.
#' 
#' Fair totalization of \eqn{R}, performed by
#' \code{rel_closure_total_fair}, is the minimal superset \eqn{R'} of \eqn{R}
#' such that if not \eqn{xRy} and not \eqn{yRx}
#' then \eqn{xR'y} and \eqn{yR'x} (see Gagolewski, 2013).
#' 
#' Even if \code{R} is transitive, the resulting relation
#' may not necessarily fulfill this property.
#' If you want a total preorder,
#' call \code{\link{rel_closure_transitive}} afterwards.
#' Missing values in \code{R} are not allowed and result in an error.
#' 
#' @param R an object coercible to a 0-1 (logical) square matrix,
#' representing a binary relation on a finite set.
#' 
#' @references
#' Gagolewski M., Scientific Impact Assessment Cannot be Fair,
#'    Journal of Informetrics 7(4), 2013, pp. 792-802.\cr
#' 
#' @return \code{rel_is_total} returns a single logical value.
#' 
#' \code{rel_closure_reflexive} returns a logical square matrix.
#' \code{\link{dimnames}} of \code{R} are preserved.
#' 
#' @family binary_relations
#' @export
#' @rdname rel_total
rel_is_total <- function(R)
{
   .Call("rel_is_total", as.matrix(R), PACKAGE="agop") # args checked internally 
}


#' @export
#' @rdname rel_total
rel_closure_total_fair <- function(R)
{
   .Call("rel_closure_total_fair", as.matrix(R), PACKAGE="agop") # args checked internally
}








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
#' Any missing value on the diagonal results in \code{NA}.
#' 
#' Reflexive closure of a binary relation \eqn{R},
#' determined by \code{rel_closure_reflexive},
#' is the minimal reflexive superset \eqn{R'} of \eqn{R}.
#' 
#' Reflexive reduction of a binary relation \eqn{R},
#' determined by \code{rel_reduction_reflexive},
#' is the minimal subset \eqn{R'} of \eqn{R},
#' such that the reflexive closures of \eqn{R} and \eqn{R'} are equal.
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



#' @title
#' Transitive Binary Relations
#' 
#' @description
#' A binary relation \eqn{R} is transitive, iff
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
#' Transitive closure of a binary relation \eqn{R},
#' determined by \code{rel_closure_transitive},
#' is the minimal superset of \eqn{R} such that it is transitive.
#' Here we use the well-known Warshall algorithm (1962),
#' which runs in \eqn{O(n^3)} time.
#' 
#' Transitive reduction of a binary relation \eqn{R},
#' determined by \code{rel_reduction_transitive},
#' is a minimal subset \eqn{R'} of \eqn{R},
#' such that the transitive closures of \eqn{R} and \eqn{R'} are equal.
#' Note that prior to calculating the reduction, we determine
#' the closure of \code{R}.
#' This function is particularly useful for draving Hasse diagrams
#' of a (pre)ordered set, see Examples.
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
#' 
#' @examples
#' \dontrun{
#' # Let ord be a total preorder (a total and transitive binary relation)
#' # === Plot the Hasse diagram of ord ===
#' # ===  requires the igraph package  ===
#' hasse <- graph.adjacency(rel_reduction_transitive(ord))
#' plot(hasse, layout=layout.fruchterman.reingold(hasse, dim=2))
#' }
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
#    if (is(B, 'igraph')) B <- get.adjacency(B)
#    stopifnot(is.matrix(B) || is(B, 'Matrix'), nrow(B) == ncol(B), nrow(B) > 0)
#    
#    # slow as hell!
#    n <- nrow(B)
#    Matrix::diag(B) <- 0
#    for (i in 1:n) {
#       for (j in 1:n) {
#          for (k in 1:n) {
#             if (as.logical(B[i,j]) &&  as.logical(B[j,k]) && as.logical(B[i,k])
#                && !as.logical(B[k,i]) && !as.logical(B[k,j]) && !as.logical(B[j,i]))
#                B[i,k] <- 0
#          }
#       }
#    }
#    B
}
