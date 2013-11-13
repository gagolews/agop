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


invisible(NULL)

# #' Choquet integral of a vector with respect to a given capacity
# #'
# #' @param x numeric vector with nonnegative elements;
# #' @param capacity function;
# #' @param sorted.dec logical;
# #' @return single numeric value
# #' @export
# int.choquet <- function(x, capacity, sorted.dec=FALSE) {
#
#    x <- as.numeric(x)
#    n <- length(x)
#    stopifnot(n >= 1)
#    stopifnot(is.finite(x), x >= 0)
#    stopifnot(is.function(capacity))
#    stopifnot(capacity(rep(FALSE, n)) == 0, capacity(rep(TRUE, n)) == 1) # boundary conditions
#
#    if (!sorted.dec) ox <- order(x, decreasing=FALSE)
#    else ox <- n:1
#
#    # if capacity is symmetric, then this could be simplified:
#    x[ox[1]]+sum(diff(x[ox])*sapply(x[ox[-1]], function(xi)
#       capacity(x >= xi)))
# }
#
#
# #' Shilkret integral of a vector with respect to a given capacity
# #'
# #' @param x numeric vector with nonnegative elements;
# #' @param capacity function;
# #' @return single numeric value
# #' @export
# int.shilkret <- function(x, capacity) {
#
#    x <- as.numeric(x)
#    n <- length(x)
#    stopifnot(n >= 1)
#    stopifnot(is.finite(x), x >= 0)
#    stopifnot(is.function(capacity))
#    stopifnot(capacity(rep(FALSE, n)) == 0, capacity(rep(TRUE, n)) == 1) # boundary conditions
#
#    # if capacity is symmetric, then this could be simplified:
#    max(x*sapply(x, function(y) capacity(x >= y)))
# }
#
#
# #' Sugeno integral of a vector with respect to a given capacity
# #'
# #' @param x numeric vector with nonnegative elements;
# #' @param measure function;
# #' @return single numeric value
# #' @export
# int.sugeno <- function(x, measure) {
#
#    x <- as.numeric(x)
#    n <- length(x)
#    stopifnot(n >= 1)
#    stopifnot(is.finite(x), x >= 0)
#    stopifnot(is.function(measure))
#    stopifnot(measure(rep(FALSE, n)) == 0) # boundary conditions
#
#    # if capacity is symmetric, then this could be simplified:
#    max(pmin(x,sapply(x, function(y) measure(x >= y))))
# }
