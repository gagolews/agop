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


#' @title Hirsch's h-index
#'
#' @description
#' Given a sequence of \eqn{n} non-negative numbers \eqn{x=(x_1,\dots,x_n)},
#' where \eqn{x_i \ge x_j \ge 0} for \eqn{i \le j},
#' the \dfn{\eqn{h}-index} (Hirsch, 2005) for \eqn{x} is defined as
#' \deqn{H(x)=\max\{i=1,\dots,n: x_i \ge i\}}{H(x)=max{i=1,\dots,n: x_i \ge i}}
#' if \eqn{n \ge 1} and \eqn{x_1 \ge 1}, or \eqn{H(x)=0} otherwise.
#'
#' @details
#' If a non-increasingly sorted vector is given, the function has O(n) run-time.
#'
#' For historical reasons, this function is also available via an alias,
#' \code{index.h} [but its usage is deprecated].
#'
#' See \code{\link{index_rp}} and \code{\link{owmax}} for natural generalizations.
#'
#' The h-index is the same as the discrete Sugeno integral of \code{x}
#' w.r.t. the counting measure (see Torra, Narukawa, 2008).
#'
#' @param x a non-negative numeric vector
#' @return a single numeric value
#'
#' @references
#' Hirsch J.E., An index to quantify individual's scientific research output,
#' \emph{Proceedings of the National Academy of Sciences} 102(46), 2005, pp. 16569-16572.
#'
#' Mesiar R., Gagolewski M., H-index and other Sugeno integrals:
#'    Some defects and their compensation,
#'  IEEE Transactions on Fuzzy Systems 24(6), 2016, pp. 1668-1672.
#' doi:10.1109/TFUZZ.2016.2516579
#'
#' Gagolewski M., Mesiar R., Monotone measures and universal integrals
#'   in a uniform framework for the scientific impact assessment problem,
#'   Information Sciences 263, 2014, pp. 166-174. doi:10.1016/j.ins.2013.12.004
#'
#' Gagolewski M., Data Fusion: Theory, Methods, and Applications,
#'    Institute of Computer Science, Polish Academy of Sciences, 2015, 290 pp.
#'    isbn:978-83-63159-20-7
#'
#' Sugeno M., \emph{Theory of fuzzy integrals and its applications},
#'    PhD thesis, Tokyo Institute of Technology, 1974.
#'
#' Torra V., Narukawa Y., The h-index and the number of citations: Two fuzzy
#' integrals, \emph{IEEE Transactions on Fuzzy Systems} 16(3), 2008, pp. 795-797.
#'
#' @examples
#' authors <- list(  # a list of numeric sequences
#'                   # (e.g. citation counts of the articles
#'                   # written by some authors)
#'     "A" =c(23,21,4,2,1,0,0),
#'     "B" =c(11,5,4,4,3,2,2,2,2,2,1,1,1,0,0,0,0),
#'     "C" =c(53,43,32,23,14,13,12,8,4,3,2,1,0)
#'  )
#' index_h(authors$A)
#' sapply(authors, index_h)
#'
#' @family impact_functions
#' @rdname index_h
#' @export
index_h <- function(x)
{
   .Call("index_h", x, PACKAGE="agop")
}


#' @rdname index_h
#' @usage index.h(x) # same as index_h(x), deprecated alias
#' @export
index.h <- index_h # deprecated





#' @title Egghe's g-index
#'
#' @description
#' Given a sequence of \eqn{n} non-negative numbers \eqn{x=(x_1,\dots,x_n)},
#' where \eqn{x_i \ge x_j \ge 0} for \eqn{i \le j},
#' the \dfn{\eqn{g}-index} (Egghe, 2006) for \eqn{x} is defined as
#' \deqn{G(x)=\max\{i=1,\dots,n: \sum_{j=1}^i x_i \ge i^2\}}{
#' G(x)=max{i=1,\dots,n: x_1+\dots+x_i \ge i^2}}
#' if \eqn{n \ge 1} and \eqn{x_1 \ge 1}, or \eqn{G(x)=0} otherwise.
#'
#' @details
#' \code{index.g} is a (deprecated) alias for \code{index_g}.
#'
#' Note that \code{index_g} is not a zero-insensitive impact function,
#' see Examples section. \code{index_g_zi} is its zero-sensitive variant:
#' it assumes that the aggregated vector is padded with zeros.
#'
#' If a non-increasingly sorted vector is given, the function has O(n) run-time.
#'
#' For historical reasons, this function is also available via an alias,
#' \code{index.g} [but its usage is deprecated].
#'
#'
#' @param x a non-negative numeric vector
#' @return a single numeric value
#'
#' @references
#' Egghe L., Theory and practise of the g-index,
#' \emph{Scientometrics} 69(1), 2006, pp. 131-152.
#'
#' Mesiar R., Gagolewski M., H-index and other Sugeno integrals:
#'    Some defects and their compensation,
#'  IEEE Transactions on Fuzzy Systems 24(6), 2016, pp. 1668-1672.
#' doi:10.1109/TFUZZ.2016.2516579
#'
#' Gagolewski M., Mesiar R., Monotone measures and universal integrals
#'   in a uniform framework for the scientific impact assessment problem,
#'   Information Sciences 263, 2014, pp. 166-174. doi:10.1016/j.ins.2013.12.004
#'
#' Gagolewski M., Data Fusion: Theory, Methods, and Applications,
#'    Institute of Computer Science, Polish Academy of Sciences, 2015, 290 pp.
#'    isbn:978-83-63159-20-7
#'
#'
#' @examples
#' sapply(list(c(9), c(9,0), c(9,0,0), c(9,0,0,0)), index_g) # not a zero-sensitive agop
#'
#' @family impact_functions
#' @rdname index_g
#' @export
index_g <- function(x)
{
   .Call("index_g", x, PACKAGE="agop")
}

#' @rdname index_g
#' @usage index.g(x) # same as index_g(x), deprecated alias
#' @export
index.g <- index_g # deprecated



#' @rdname index_g
#' @export
index_g_zi <- function(x)
{
   .Call("index_g_zi", x, PACKAGE="agop")
}



#' @title Kosmulski's MAXPROD-index
#'
#' @description
#' Given a sequence of \eqn{n} non-negative numbers \eqn{x=(x_1,\dots,x_n)},
#' where \eqn{x_i \ge x_j \ge 0} for \eqn{i \le j},
#' the \dfn{MAXPROD-index} (Kosmulski, 2007) for \eqn{x} is defined as
#' \deqn{MAXPROD(x)=\max\{i x_i: i=1,\dots,n\}}{MAXPROD(x)=max{i x_i: i=1,\dots,n}}
#'
#' @details
#' If a non-increasingly sorted vector is given, the function has O(n) run-time.
#'
#' The MAXPROD index is the same as the discrete Shilkret integral of \code{x}
#' w.r.t. the counting measure.
#'
#' See \code{\link{index_lp}} for a natural generalization.
#'
#' @param x a non-negative numeric vector
#' @return a single numeric value
#'
#' @references
#' Kosmulski M., MAXPROD - A new index for assessment of the scientific output
#' of an individual, and a comparison with the h-index,
#' \emph{Cybermetrics} 11(1), 2007.
#'
#' Mesiar R., Gagolewski M., H-index and other Sugeno integrals:
#'    Some defects and their compensation,
#'  IEEE Transactions on Fuzzy Systems 24(6), 2016, pp. 1668-1672.
#' doi:10.1109/TFUZZ.2016.2516579
#'
#' Gagolewski M., Mesiar R., Monotone measures and universal integrals
#'   in a uniform framework for the scientific impact assessment problem,
#'   Information Sciences 263, 2014, pp. 166-174. doi:10.1016/j.ins.2013.12.004
#'
#' Gagolewski M., Data Fusion: Theory, Methods, and Applications,
#'    Institute of Computer Science, Polish Academy of Sciences, 2015, 290 pp.
#'    isbn:978-83-63159-20-7
#'
#'
#' @family impact_functions
#' @rdname index_maxprod
#' @export
index_maxprod <- function(x)
{
   .Call("index_maxprod", x, PACKAGE="agop")
}



#' @title Woeginger's w-index
#'
#' @description
#' Given a sequence of \eqn{n} non-negative numbers \eqn{x=(x_1,\dots,x_n)},
#' where \eqn{x_i \ge x_j \ge 0} for \eqn{i \le j},
#' the \dfn{\eqn{w}-index} (Woeginger, 2008) for \eqn{x} is defined as
#' \deqn{W(x)=\max\{i=1,\dots,n: x_{j}\ge i-j+1, \forall j=1,\dots,i\}}{
#' W(x)=max{i=1,\dots,n: x_j >= i-j+1 for all j=1,\dots,i}}
#'
#' @details
#' If a non-increasingly sorted vector is given, the function has O(n) run-time.
#'
#' See \code{\link{index_rp}} for a natural generalization.
#'
#' @param x a non-negative numeric vector
#' @return a single numeric value
#'
#' @references
#' Woeginger G. J., An axiomatic characterization of the Hirsch-index.
#' \emph{Mathematical Social Sciences} 56(2), 2008, pp. 224-232.
#'
#'
#' @family impact_functions
#' @rdname index_w
#' @export
index_w <- function(x)
{
   .Call("index_w", x, PACKAGE="agop")
}


#' @title
#' The r_p-index
#'
#' @description
#' Given a sequence of \eqn{n} non-negative numbers \eqn{x=(x_1,\dots,x_n)},
#' where \eqn{x_i \ge x_j} for \eqn{i \le j},
#' the \dfn{\eqn{r_p}-index} for \eqn{p=\infty} equals to
#' \deqn{r_p(x)=\max_{i=1,\dots,n} \{ \min\{i,x_i\} \}}{
#' r_p(x) = max{ min{i, x_i} } for i=1,\dots,n}
#' if \eqn{n \ge 1}, or \eqn{r_\infty(x)=0} otherwise.
#' That is, it is equivalent to a particular OWMax operator,
#' see \code{\link{owmax}}.
#'
#' For the definition of the \eqn{r_p}-index for \eqn{p < \infty} we refer
#' to (Gagolewski, Grzegorzewski, 2009).
#'
#' @details
#' Note that if \eqn{x_1,\dots,x_n} are integers, then
#' \deqn{r_\infty(x)=H(x),} where \eqn{H} is the \eqn{h}-index (Hirsch, 2005) and
#' \deqn{r_1(x)=W(x),} where \eqn{W} is the \eqn{w}-index (Woeginger, 2008),
#' see \code{\link{index_h}} and \code{\link{index_w}}.
#'
#' If a non-increasingly sorted vector is given, the function has O(n) run-time.
#'
#' For historical reasons, this function is also available via an alias, \code{index.rp}
#'  [but its usage is deprecated].
#'
#' @references
#' Gagolewski M., Grzegorzewski P., A geometric approach to the construction
#' of scientific impact indices, \emph{Scientometrics} 81(3), 2009, pp. 617-634.
#'
#' Hirsch J.E., An index to quantify individual's scientific research output,
#' \emph{Proceedings of the National Academy of Sciences} 102(46), 2005, pp. 16569-16572.
#'
#' Woeginger G.J., An axiomatic characterization of the Hirsch-index,
#' \emph{Mathematical Social Sciences} 56(2), 2008, pp. 224-232.
#'
#' @param x a non-negative numeric vector
#' @param p index order, \eqn{p \in [1,\infty]}{p in [1,\infty]}; defaults \eqn{\infty} (\code{Inf}).
#' @return a single numeric value
#' @examples
#' x <- runif(100, 0, 100);
#' index.rp(x);            # the r_oo-index
#' floor(index.rp(x));     # the h-index
#' index.rp(floor(x), 1);  # the w-index
#' @family impact_functions
#' @rdname index_rp
#' @export
index_rp <- function(x, p=Inf)
{
   .Call("index_rp", x, p, PACKAGE="agop")
}



#' @rdname index_rp
#' @usage index.rp(x, p = Inf) # same as index_rp(x, p), deprecated alias
#' @export
index.rp <- index_rp # deprecated




#' @title
#' The l_p-index
#'
#' @description
#' Given a sequence of \eqn{n} non-negative numbers \eqn{x=(x_1,\dots,x_n)},
#' where \eqn{x_i \ge x_j} for \eqn{i \le j},
#' the \dfn{\eqn{l_p}-index} for \eqn{p=\infty} equals to
#' \deqn{l_p(x)=\arg\max_{(i,x_i), i=1,\dots,n} \{ i x_i \}}{
#' l_p(x) = arg max_(i,x_i) { i*x_i } for i=1,\dots,n}
#' if \eqn{n \ge 1}, or \eqn{l_\infty(x)=0} otherwise.
#' Note that if \eqn{(i,x_i)=l_\infty(x)}, then
#' \deqn{MAXPROD(x) = \mathtt{prod}(l_\infty(x)) = i x_i,}{
#' MAXPROD(x) = prod(l_\infty(x)) = i*x_i,}
#' where \eqn{MAXPROD} is the index proposed in (Kosmulski, 2007),
#' see \code{\link{index_maxprod}}.
#' Moreover, this index corresponds to the Shilkret integral
#' of \eqn{x} w.r.t. some monotone measure,
#' cf. (Gagolewski, Debski, Nowakiewicz, 2013).
#'
#' For the definition of the \eqn{l_p}-index for \eqn{p < \infty} we refer
#' to (Gagolewski, Grzegorzewski, 2009a).
#'
#' @details
#' The \eqn{l_p}-index, by definition, is not an impact function, as
#' it produces 2 numeric values. Thus, it should be projected to one dimension.
#' However, you may set the \code{projection} argument
#' to \code{\link{identity}} so as to obtain the 2-dimensional index
#'
#' If a non-increasingly sorted vector is given, the function has  O(n) run-time
#' for any \eqn{p}, see (Gagolewski, Debski, Nowakiewicz, 2013).
#'
#' For historical reasons, this function is also available via an alias,
#' \code{index.lp} [but its usage is deprecated].
#'
#' @references
#' Gagolewski M., Grzegorzewski P., A geometric approach to the construction
#' of scientific impact indices, \emph{Scientometrics} 81(3), 2009a, pp. 617-634.
#'
#' Gagolewski M., Debski M., Nowakiewicz M.,
#'    \emph{Efficient Algorithm for Computing Certain Graph-Based Monotone
#'    Integrals: the lp-Indices}, In: Mesiar R., Bacigal T. (Eds.),
#'    \emph{Proc. Uncertainty Modelling}, STU Bratislava,
#'    ISBN:978-80-227-4067-8, 2013, pp. 17-23.
#'
#' Kosmulski M., MAXPROD - A new index for assessment of the scientific output
#'  of an individual, and a comparison with the h-index, \emph{Cybermetrics} 11(1), 2007.
#'
#'  Shilkret, N., Maxitive measure and integration,
#'    \emph{Indag. Math.} 33, 1971, pp. 109-116.
#'
#' @param x a non-negative numeric vector
#' @param p index order, \eqn{p \in [1,\infty]}{p in [1,\infty]}; defaults \eqn{\infty} (\code{Inf}).
#' @param projection function
#' @return result of \code{projection}(\code{c}(\eqn{i, x_i}))
#' @examples
#' x <- runif(100, 0, 100)
#' index.lp(x, Inf, identity)  # two-dimensional value, can not be used
#'                             # directly in the analysis
#' index.lp(x, Inf, prod)      # the MAXPROD-index (one-dimensional) [default]
#' @family impact_functions
#' @rdname index_lp
#' @export
index_lp <- function(x, p=Inf, projection=prod)
{
   projection(.Call("index_lp", x, p, PACKAGE="agop"))
}



#' @rdname index_lp
#' @usage index.lp(x, p = Inf, projection = prod)  # deprecated alias
#' @export
index.lp <- index_lp # deprecated
