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
#' t-norms
#'
#' @description
#' Various t-norms.
#' Each of these is a fuzzy logic generalization
#' of the classical conjuntion operation.
#'
#' @details
#' The minimum t-norm is given by \eqn{T_M(x,y)=min(x, y)}.
#'
#' The product t-norm is given by \eqn{T_P(x,y)=xy}.
#'
#' The Lukasiewicz t-norm is given by \eqn{T_L(x,y)=max(x+y-1,0)}.
#'
#' The drastic t-norm is given by \eqn{T_D(x,y)=0} iff
#' \eqn{x,y\in [0,1)} and \eqn{min(x, y)} otherwise.
#'
#' The Fodor t-norm is given by \eqn{T_F(x,y)=0}
#' iff \eqn{x+y \le 1} and \eqn{min(x, y)} otherwise.
#'
#'
#' @param x numeric vector with elements in \eqn{[0,1]}
#' @param y numeric vector of the same length as \code{y}, with elements in \eqn{[0,1]}
#' @return
#' Numeric vector of the same length as \code{x} and \code{y}.
#' The \code{i}th element of the resulting vector gives the result
#' of calculating \code{T(x[i], y[i])}.
#'
#' @rdname fuzzylogic_tnorm
#' @export
#' @family fuzzy_logic
#' @references
#' Klir G.J, Yuan B., \emph{Fuzzy sets and fuzzy logic. Theory and applications},
#' Prentice Hall PTR, New Jersey, 1995.
tnorm_minimum <- function(x, y) {
   .Call("tnorm_minimum", x, y, PACKAGE="agop")
}


#' @rdname fuzzylogic_tnorm
#' @export
tnorm_product <- function(x, y) {
   .Call("tnorm_product", x, y, PACKAGE="agop")
}

#' @rdname fuzzylogic_tnorm
#' @export
tnorm_lukasiewicz <- function(x, y) {
   .Call("tnorm_lukasiewicz", x, y, PACKAGE="agop")
}

#' @rdname fuzzylogic_tnorm
#' @export
tnorm_drastic <- function(x, y) {
   .Call("tnorm_drastic", x, y, PACKAGE="agop")
}

#' @rdname fuzzylogic_tnorm
#' @export
tnorm_fodor <- function(x, y) {
   .Call("tnorm_fodor", x, y, PACKAGE="agop")
}