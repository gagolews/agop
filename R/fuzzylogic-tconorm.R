## This file is part of the 'agop' library.
##
## Copyright 2013-2019 Marek Gagolewski, Anna Cena
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
#' t-conorms
#'
#' @description
#' Various t-conorms.
#' Each of these is a fuzzy logic generalization
#' of the classical alternative operation.
#'
#' @details
#' A function \eqn{S: [0,1]\times [0,1]\to [0,1]}
#' is a \emph{t-conorm} if for all \eqn{x,y,z\in [0,1]} it holds:
#' (a) \eqn{S(x,y)=S(y,x)};
#' (b) if \eqn{y\le z}, then \eqn{S(x,y)\le S(x,z)};
#' (c) \eqn{S(x,S(y,z))=S(S(x,y),z)};
#' (d) \eqn{S(x, 0)=x}.
#'
#' The minimum t-conorm is given by \eqn{S_M(x,y)=max(x, y)}.
#'
#' The product t-conorm is given by \eqn{S_P(x,y)=x+y-xy}.
#'
#' The Lukasiewicz t-conorm is given by \eqn{S_L(x,y)=min(x+y,1)}.
#'
#' The drastic t-conorm is given by \eqn{S_D(x,y)=1} iff
#' \eqn{x,y\in (0,1]}, and \eqn{max(x, y)} otherwise.
#'
#' The Fodor t-conorm is given by \eqn{S_F(x,y)=1}
#' iff \eqn{x+y \ge 1}, and \eqn{max(x, y)} otherwise.
#'
#'
#' @param x numeric vector with elements in \eqn{[0,1]}
#' @param y numeric vector of the same length as \code{x},
#'  with elements in \eqn{[0,1]}
#' @return
#' Numeric vector of the same length as \code{x} and \code{y}.
#' The \code{i}th element of the resulting vector gives the result
#' of calculating \code{S(x[i], y[i])}.
#'
#' @rdname fuzzylogic_tconorm
#' @export
#' @family fuzzy_logic
#' @references
#' Klir G.J, Yuan B., \emph{Fuzzy sets and fuzzy logic. Theory and applications},
#' Prentice Hall PTR, New Jersey, 1995.
#'
#' Gagolewski M., Data Fusion: Theory, Methods, and Applications,
#'    Institute of Computer Science, Polish Academy of Sciences, 2015, 290 pp.
#'    isbn:978-83-63159-20-7
tconorm_minimum <- function(x, y) {
   .Call("tconorm_minimum", x, y, PACKAGE="agop")
}


#' @rdname fuzzylogic_tconorm
#' @export
tconorm_product <- function(x, y) {
   .Call("tconorm_product", x, y, PACKAGE="agop")
}

#' @rdname fuzzylogic_tconorm
#' @export
tconorm_lukasiewicz <- function(x, y) {
   .Call("tconorm_lukasiewicz", x, y, PACKAGE="agop")
}

#' @rdname fuzzylogic_tconorm
#' @export
tconorm_drastic <- function(x, y) {
   .Call("tconorm_drastic", x, y, PACKAGE="agop")
}

#' @rdname fuzzylogic_tconorm
#' @export
tconorm_fodor <- function(x, y) {
   .Call("tconorm_fodor", x, y, PACKAGE="agop")
}
