## This file is part of the 'agop' library.
##
## Copyright 2013-2014 Marek Gagolewski, Anna Cena
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
#' Checks whether a given numeric vector
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
#'
#' @return Returns a single logical value
#' indicating whether \code{x} is weakly
#' dominated by \code{y}.
#'
#' @family binary_relations
#' @export
pord_nd <- function(x, y)
{
   .Call("pord_nd", x, y, PACKAGE="agop")
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
#' @family binary_relations
#' @family impact_functions
#' @export
pord_weakdom <- function(x, y)
{
   .Call("pord_weakdom", x, y, PACKAGE="agop")
}


#' @title
#' Compare Spreads of Vectors (Preorder)
#'
#' @description
#' These functions determine whether
#' one numeric vector has not greater spread than the
#' other
#'
#'
#' @details
#' These functions accept only vectors of the same size.
#' [TO DO: should not it return \code{FALSE} or \code{NA} in this case?]
#'
#' We say that \bold{x} of size \eqn{n}
#' is of \emph{no greater spread} than \bold{y}
#' iff for all \eqn{i,j=1,\dots,n} such
#' that \eqn{x_i>x_j} it holds \eqn{x_i-x_j\le y_i-y_jll
#' }.
#' Such a preorder is used in the definition of
#' dispersion functions (see Gagolewski, 2013)
#' and is implemented in \code{pord_spread},
#'
#'
#' Moreover, \code{pord_spreadsym} implements
#' the relation corresponding to symmetrized dispersion
#' functions, i.e. which acts on sorted vectors.
#'
#' Note that the class of dispersion functions includes
#' e.g. the sample variance (see \code{\link{var}}),
#' standard veriation (see  \code{\link{sd}}),
#' range (see  \code{\link{range}} and then  \code{\link{diff}}),
#' interquartile range (see  \code{\link{IQR}}),
#' median absolute deviation (MAD).
#'
#'
#' @param x numeric vector
#' @param y numeric vector of the same length as \code{x}
#'
#' @return Both functions return a single logical value,
#' which states whether \code{x} has no greater
#' spread than \code{y}
#'
#' @references
#' Gagolewski M., \emph{Dispersion Functions: Aggregation Operators that
#'    Measure Variability, Spread, or Scatter of Numeric Sequences},
#'    submitted paper, 2013.
#'
#' Gagolewski M., \emph{Symmetric Dispersion Functions}, in preparation, 2013.
#'
#' @family binary_relations
#' @family dispersion_functions
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
