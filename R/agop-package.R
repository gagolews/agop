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



#' @title Aggregation Operators and Preordered Sets Package for R
#'
#' @author
#' Marek Gagolewski [aut,cre],\cr
#' Anna Cena [ctb]
#'
#' @description
#' \bold{Keywords}: aggregation, bibliometrics, scientometrics, scientific impact,
#' webometrics, preorders, binary relations, means, OWA, OWMax, OWMin, Hirsch's h-index,
#' Egghe's g-index, variance, spread, decision making, fuzzy logic.
#'
#' \bold{Acknowledgments}:
#' The development of the package in March-June 2013 was partially supported
#' by the European Union from the resources of the European Social Fund, Project PO KL
#' ``Information technologies: Research and their interdisciplinary
#' applications'', agreement UDA-POKL.04.01.01-00-051/10-00.
#'
#' @useDynLib agop
#' @name agop-package
#' @import stats
#' @import grDevices
#' @import graphics
#' @docType package
#' @importFrom stats qf
#' @importFrom stats pf
#' @importFrom stats runif
#' @importFrom stats splinefun
#' @importFrom stats optim
#' @importFrom stats uniroot
#' @importFrom graphics plot
#' @importFrom graphics par
#' @importFrom graphics segments
#' @importFrom graphics points
invisible(NULL)
