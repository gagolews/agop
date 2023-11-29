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
#' Fuzzy Negations
#'
#' @description
#' Various fuzzy negations.
#' Each of these is a fuzzy logic generalization
#' of the classical negation operation.
#'
#' @details
#' A function \eqn{N: [0,1]\to [0,1]}
#' is a \emph{fuzzy implication} if for all \eqn{x,y\in [0,1]} it holds:
#' (a) if \eqn{x\le y}, then \eqn{N(x)\ge N(y)};
#' (b) \eqn{N(1)=0};
#' (c) \eqn{N(0)=1}.
#'
#' The classic fuzzy negation is given by \eqn{N_C(x)=1-x}.
#'
#' The Yager fuzzy negation is given by \eqn{N_Y(x)=sqrt(1-x^2)}.
#'
#' The minimal fuzzy negation is given by \eqn{N_0(x,y)=1} iff
#' \eqn{x=0}, and \eqn{0} otherwise.
#'
#' The maximal fuzzy negation is given by \eqn{N_1(x,y)=1}
#' iff \eqn{x<1}, and \eqn{0} otherwise.
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
#'
#' Gagolewski M., Data Fusion: Theory, Methods, and Applications,
#'    Institute of Computer Science, Polish Academy of Sciences, 2015, 290 pp.
#'    isbn:978-83-63159-20-7
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
