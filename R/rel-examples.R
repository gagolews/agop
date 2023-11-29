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
#' Weak Dominance Relation (Preorder)
#'
#' @description
#' Checks whether a numeric vector
#' of arbitrary length is (weakly) dominated (elementwise)
#' by another vector of the same length.
#'
#' @details
#' We say that a numeric vector \bold{x}
#' of length \eqn{n_x}
#' is \emph{weakly dominated} by \bold{y}
#' of length \eqn{n_y}
#' iff
#' \enumerate{
#' \item \eqn{n_x=n_y} and
#' \item for all \eqn{i=1,\dots,n_x} it holds
#'    \eqn{x_i\le y_i}.
#' }
#'
#' This relation is a preorder: it is reflexive (see \code{\link{rel_is_reflexive}})
#' and transitive (see \code{\link{rel_is_transitive}}),
#' but not necessarily total  (see \code{\link{rel_is_total}}).
#' See \code{\link{rel_graph}} for a convenient function
#' to calculate the relationship between all pairs of elements
#' of a given set.
#'
#' Such a preorder is tightly related to classical aggregation functions:
#' each aggregation function is a morphism between
#' weak-dominance-preordered set of vectors
#' and the set of reals equipped with standard linear ordering.
#'
#'
#' @param x numeric vector with nonnegative elements
#' @param y numeric vector with nonnegative elements
#' @param incompatible_lengths single logical value,
#' value to return iff lengths of \code{x} and \code{y} differ
#'
#' @return Returns a single logical value
#' indicating whether \code{x} is weakly
#' dominated by \code{y}.
#'
#' @references
#' Grabisch M., Marichal J.-L., Mesiar R., Pap E., \emph{Aggregation functions},
#'    Cambridge University Press, 2009.
#'
#' Gagolewski M., Data Fusion: Theory, Methods, and Applications,
#'    Institute of Computer Science, Polish Academy of Sciences, 2015, 290 pp.
#'    isbn:978-83-63159-20-7
#'
#' @family binary_relations
#' @export
pord_nd <- function(x, y, incompatible_lengths=NA)
{
   .Call("pord_nd", x, y, incompatible_lengths, PACKAGE="agop")
}



#' @title
#' Weak Dominance Relation (Preorder) in the Producer Assessment Problem
#'
#' @description
#' Checks whether a given numeric vector
#' of arbitrary length is (weakly) dominated
#' by another vector, possibly of different length,
#' in terms of (sorted) elements' values and their number.
#'
#' @details
#' We say that a numeric vector \bold{x}
#' of length \eqn{n_x}
#' is \emph{weakly dominated} by \bold{y}
#' of length \eqn{n_y}
#' iff
#' \enumerate{
#' \item \eqn{n_x\le n_y} and
#' \item for all \eqn{i=1,\dots,n} it holds
#'    \eqn{x_{(n_x-i+1)}\le y_{(n_y-i+1)}}.
#' }
#'
#' This relation is a preorder: it is reflexive (see \code{\link{rel_is_reflexive}})
#' and transitive (see \code{\link{rel_is_transitive}}),
#' but not necessarily total  (see \code{\link{rel_is_total}}).
#' See \code{\link{rel_graph}} for a convenient function
#' to calculate the relationship between all pairs of elements
#' of a given set.
#'
#' Note that this dominance relation gives the same value
#' for all permutations of input vectors' element.
#' Such a preorder is tightly related to symmetric impact functions:
#' each impact function is a morphism between
#' weak-dominance-preordered set of vectors
#' and the set of reals equipped with standard linear ordering
#' (see Gagolewski, Grzegorzewski, 2011
#' and Gagolewski, 2013).
#'
#'
#' @param x numeric vector with nonnegative elements
#' @param y numeric vector with nonnegative elements
#'
#' @return Returns a single logical value
#' indicating whether \code{x} is weakly
#' dominated by \code{y}.
#'
#' @references
#' Gagolewski M., Grzegorzewski P., Possibilistic Analysis of Arity-Monotonic
#' Aggregation Operators and Its Relation to Bibliometric Impact Assessment of Individuals,
#' \emph{International Journal of Approximate Reasoning} 52(9), 2011, pp. 1312-1324.
#'
#' Gagolewski M., Scientific Impact Assessment Cannot be Fair,
#' \emph{Journal of Informetrics} 7(4), 2013, pp. 792-802.
#'
#' Gagolewski M., Data Fusion: Theory, Methods, and Applications,
#'    Institute of Computer Science, Polish Academy of Sciences, 2015, 290 pp.
#'    isbn:978-83-63159-20-7
#'
#' @family binary_relations
#' @family impact_functions
#' @export
pord_weakdom <- function(x, y)
{
   .Call("pord_weakdom", x, y, PACKAGE="agop")
}


#' @title
#' Compare Spread of Vectors (Preorder)
#'
#' @description
#' This function determines whether
#' one numeric vector has not greater spread than the
#' other
#'
#'
#' @details
#' We say that \bold{x} of size \eqn{n}
#' is of \emph{no greater spread} than \bold{y}
#' iff for all \eqn{i,j=1,\dots,n} such
#' that \eqn{x_i>x_j} it holds \eqn{x_i-x_j\le y_i-y_jll
#' }.
#' Such a preorder is used in the definition of
#' a spread measure (see Gagolewski, 2015).
#'
#' Note that the class of dispersion functions includes
#' e.g. the sample variance (see \code{\link{var}}),
#' standard veriation (see  \code{\link{sd}}),
#' range (see  \code{\link{range}} and then  \code{\link{diff}}),
#' interquartile range (see  \code{\link{IQR}}),
#' median absolute deviation (see \code{\link{mad}}).
#'
#'
#' @param x numeric vector
#' @param y numeric vector of the same length as \code{x}
#' @param incompatible_lengths single logical value,
#' value to return iff lengths of \code{x} and \code{y} differ
#'
#' @return The function returns a single logical value,
#' which states whether \code{x} has no greater
#' spread than \code{y}
#'
#' @references
#' Gagolewski M., Spread measures and their relation to aggregation functions,
#' \emph{European Journal of Operational Research} 241(2), 2015, pp. 469--477.
#'
#' Gagolewski M., Data Fusion: Theory, Methods, and Applications,
#'    Institute of Computer Science, Polish Academy of Sciences, 2015, 290 pp.
#'    isbn:978-83-63159-20-7
#'
#' @family binary_relations
#' @family spread_measures
#' @export
pord_spread <- function(x, y, incompatible_lengths=NA)
{
   .Call("pord_spread", x, y, incompatible_lengths, PACKAGE="agop")
}
