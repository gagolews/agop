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
#' Fuzzy Implications
#'
#' @description
#' Various fuzzy implications
#' Each of these is a fuzzy logic generalization
#' of the classical implication operation.
#'
#' @details
#' A function \eqn{I: [0,1]\times [0,1]\to [0,1]}
#' is a \emph{fuzzy implication} if for all \eqn{x,y,x',y'\in [0,1]} it holds:
#' (a) if \eqn{x\le x'}, then \eqn{I(x, y)\ge I(x', y)};
#' (b) if \eqn{y\le y'}, then \eqn{I(x, y)\le I(x, y')};
#' (c) \eqn{I(1, 1)=1};
#' (d) \eqn{I(0, 0)=1};
#' (e) \eqn{I(1, 0)=0}.
#'
#' The minimal fuzzy implication is given by \eqn{I_0(x, y)=1}
#' iff \eqn{x=0} or \eqn{y=1}, and 0 otherwise.
#'
#' The maximal fuzzy implication is given by \eqn{I_1(x, y)=0}
#' iff \eqn{x=1} and \eqn{y=0}, and 1 otherwise.
#'
#' The Kleene-Dienes fuzzy implication is given by \eqn{I_{KD}(x, y)=max(1-x, y)}.
#'
#' The Lukasiewicz fuzzy implication is given by \eqn{I_{L}(x, y)=min(1-x+y, 1)}.
#'
#' The Reichenbach fuzzy implication is given by \eqn{I_{RB}(x, y)=1-x+xy}.
#'
#'
#' @param x numeric vector with elements in \eqn{[0,1]}
#' @param y numeric vector of the same length as \code{x},
#'  with elements in \eqn{[0,1]}
#' @return
#' Numeric vector of the same length as \code{x} and \code{y}.
#' The \code{i}th element of the resulting vector gives the result
#' of calculating \code{I(x[i], y[i])}.
#'
#' @rdname fuzzylogic_implication
#' @export
#' @family fuzzy_logic
#' @references
#' Klir G.J, Yuan B., \emph{Fuzzy sets and fuzzy logic. Theory and applications},
#' Prentice Hall PTR, New Jersey, 1995.
fimplication_minimal <- function(x, y) {
   .Call("fimplication_minimal", x, y, PACKAGE="agop")
}


#' @rdname fuzzylogic_implication
#' @export
fimplication_maximal <- function(x, y) {
   .Call("fimplication_maximal", x, y, PACKAGE="agop")
}


#' @rdname fuzzylogic_implication
#' @export
fimplication_kleene <- function(x, y) {
   .Call("fimplication_kleene", x, y, PACKAGE="agop")
}


#' @rdname fuzzylogic_implication
#' @export
fimplication_lukasiewicz <- function(x, y) {
   .Call("fimplication_lukasiewicz", x, y, PACKAGE="agop")
}


#' @rdname fuzzylogic_implication
#' @export
fimplication_reichenbach <- function(x, y) {
   .Call("fimplication_reichenbach", x, y, PACKAGE="agop")
}
