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
#' Fuzzy Negations
#'
#' @description
#' Various fuzzy negations.
#' Each of these is a fuzzy logic generalization
#' of the classical negation operation.
#'
#' @details
#' The classic fuzzy negation is given by \eqn{N_C(x)=1-x}.
#'
#' The Yager fuzzy negation is given by \eqn{N_Y(x)=sqrt(1-x^2)}.
#'
#' The minimal fuzzy negation is given by \eqn{N_0(x,y)=1} iff
#' \eqn{x=0} and \eqn{0} otherwise.
#'
#' The maximal fuzzy negation is given by \eqn{N_1(x,y)=1}
#' iff \eqn{x<1} and \eqn{0} otherwise.
#'
#'
#' @param x numeric vector with elements in \eqn{[0,1]}
#' @return
#' Numeric vector of the same length as \code{x}.
#' The \code{i}th element of the resulting vector gives the result
#' of calculating \code{N(x[i])}.
#'
#' @rdname fuzzylogic_negation
#' @export
#' @family fuzzy_logic
#' @references
#' Klir G.J, Yuan B., \emph{Fuzzy sets and fuzzy logic. Theory and applications},
#' Prentice Hall PTR, New Jersey, 1995.
fnegation_yager <- function(x) {
   .Call("fnegation_yager", x, PACKAGE="agop")
}


#' @rdname fuzzylogic_negation
#' @export
fnegation_classic <- function(x) {
   .Call("fnegation_classic", x, PACKAGE="agop")
}


#' @rdname fuzzylogic_negation
#' @export
fnegation_minimal <- function(x) {
   .Call("fnegation_minimal", x, PACKAGE="agop")
}


#' @rdname fuzzylogic_negation
#' @export
fnegation_maximal <- function(x) {
   .Call("fnegation_maximal", x, PACKAGE="agop")
}