## This file is part of the 'agop' library.
##
## Copyright 2013-2014 Marek Gagolewski, Anna Cena
##
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



#' @title Aggregation Operators and Preordered Sets Package for R
#'
#' @description
#' ``The process of combining several numerical values into a single
#' representative one is called aggregation, and the numerical function
#' performing this process is called aggregation function.
#' This simple definition demonstrates the size of the field of application
#' of aggregation: applied mathematics (e.g. probability, statistics,
#' decision theory), computer science (e.g. artificial intelligence,
#' operation research), as well as many applied fields (economics and finance,
#' pattern recognition and image processing, data fusion,
#' multicriteria decision making, automated reasoning etc.).
#' Although history of aggregation is probably
#' as old as mathematics (think of the arithmetic mean), its existence
#' has reminded underground till only recent (...).''
#' (Grabisch et al., 2009, p. xiii)
#'
#' @details
#' \pkg{agop} is an open source (LGPL 3) package for R,
#' to which anyone can contribute.
#' It started as a fork of the \pkg{CITAN}
#' package (Gagolewski, 2011).
#'
#'
#' For more information refer to the Package Vignette.
#'
#' @section Package Facilities:
#'
#' \itemize{
#'    \item Predefined classes of aggregation operators:
#'    \itemize{
#'       \item \code{\link{wam}}, \code{\link{owa}},
#'       \code{\link{wmax}}, \code{\link{wmin}},
#'       \code{\link{owmax}}, \code{\link{owmin}} -- classical
#'       aggregations functions;
#'       \item \code{\link{index_h}}, \code{\link{index_g}},
#'       \code{\link{index_g_zi}}, \code{\link{index_maxprod}},
#'       \code{\link{index_h}}, \code{\link{index_w}},
#'       \code{\link{index_lp}}, \code{\link{index_rp}}
#'         -- impact functions
#'       useful in the Producers Assessment Problem;
#'       \item \code{\link{d2owa}} -- spread measures;
#'       for other see also \code{\link{var}},
#'       \code{\link{sd}}, and \code{\link{range}};
#'       \item \code{\link{tnorm_minimum}}, \code{\link{tnorm_lukasiewicz}},
#'       etc. -- fuzzy logic connectives
#'    }
#'    \item Functions dealing with preorders and other binary relations
#'    on finite sets:
#'    \itemize{
#'       \item \code{\link{pord_weakdom}},
#'       \code{\link{pord_spread}}, \code{\link{pord_spreadsym}} --
#'       particular preorders in the space of numeric vectors;
#'       see also \code{\link{rel_graph}} for computing a binary
#'       relation for a given set of vectors;
#'       \item TO DO.....
#'       \item TO DO.....
#'    }
#'    \item Statistical distributions:
#'    \itemize{
#'       \item \code{\link{ppareto2}}, \code{\link{dpareto2}},
#'       \code{\link{dpareto2}}, \code{\link{rpareto2}}
#'       -- (continuous) Pareto Type-II distribution;
#'       see \code{\link{pareto2_estimate_mle}}
#'       and \code{\link{pareto2_estimate_mmse}}
#'       for parameter estimation,
#'       \code{\link{pareto2_test_ad}} for a goodness-of-fit
#'       test, and \code{\link{pareto2_test_f}} for
#'       a two-sample test for equality of the shape parameter;
#'       \item \code{\link{pdpareto2}}, \code{\link{ddpareto2}},
#'       \code{\link{ddpareto2}}, \code{\link{rdpareto2}}
#'       -- (discretized) Pareto Type-II distribution;
#'       see \code{\link{dpareto2_estimate_mle}} for
#'       parameter estimation;
#'       \item \code{\link{exp_test_ad}} -- Anderson-Darling
#'       (approximate) goodness-of-fit test for the
#'       Exponential distribution;
#'    }
#'    \item Data visualization:
#'    \itemize{
#'       \item \code{\link{plot_producer}} -- draws a graphical
#'       representation of a numeric vector.
#'    }
#' }
#'
#' @author
#' Marek Gagolewski \email{gagolews@@rexamine.com} [aut,cre],\cr
#' Anna Cena \email{cena@@rexamine.com} [ctb]
#'
#' \bold{Keywords}: aggregation, bibliometrics, scientometrics, scientific impact,
#' webometrics, preorders, binary relations, means, OWA, OWMax, OWMin, Hirsch's h-index,
#' Egghe's g-index, variance, spread, decision making, fuzzy logic.
#'
#' \bold{Acknowledgments}:
#' The development of the package in March-June 2013 was partially supported
#' by the European Union from resources of the European Social Fund, Project PO KL
#' ``Information technologies: Research and their interdisciplinary
#' applications'', agreement UDA-POKL.04.01.01-00-051/10-00.
#'
#' @useDynLib agop
#' @name agop-package
#' @import stats
#' @import grDevices
#' @import graphics
#' @docType package
#' @references
#' Beliakov G., Pradera A., Calvo T., \emph{Aggregation Functions:
#'    A Guide for Practitioners}, Springer-Verlag, 2007.
#'
#' Cena A., Gagolewski M.,
#' OM3: Ordered maxitive, minitive, and modular aggregation operators --
#'  axiomatic and probabilistic properties in an arity-monotonic setting,
#'  \emph{Fuzzy Sets and Systems}, 2014, doi:10.1016/j.fss.2014.04.001.
#'
#' Cena A., Gagolewski M., \emph{OM3: ordered maxitive, minitive,
#'    and modular aggregation operators
#'    - Part I: Axiomatic analysis under arity-dependence}, In: Bustince H. et al (Eds.),
#'    \emph{Aggregation Functions in Theory and in Practise} (AISC 228),
#'    Springer-Verlag, Heidelberg, 2013, pp. 93-103.
#'
#' Cena A., Gagolewski M., \emph{OM3: ordered maxitive, minitive, and modular aggregation operators
#'    - Part II: A simulation study}, In: Bustince H. et al (Eds.),
#'    \emph{Aggregation Functions in Theory and in Practise} (AISC 228),
#'    Springer-Verlag, Heidelberg, 2013, pp. 105-115.
#'
#' Choquet G., Theory of capacities, \emph{Annales de l'institut Fourier} 5,
#'    1954, pp. 131-295.
#'
#' Dubois D., Prade H., Testemale C., Weighted fuzzy pattern matching,
#'    \emph{Fuzzy Sets and Systems} 28, 1988, pp. 313-331.
#'
#' Dubois D., Prade H., Semantics of quotient operators in fuzzy
#'    relational databases, \emph{Fuzzy Sets and Systems} 78(1), 1996, pp. 89-93.
#'
#' Gagolewski M., Spread measures and their relation to aggregation functions,
#' \emph{European Journal of Operational Research}, 2014, doi:10.1016/j.ejor.2014.08.034.
#'
#' Gagolewski M., Scientific Impact Assessment Cannot be Fair,
#'    \emph{Journal of Informetrics} 7(4), 2013, pp. 792-802.
#'
#' Gagolewski M., On the Relationship Between Symmetric Maxitive, Minitive,
#'    and Modular Aggregation Operators,
#'    \emph{Information Sciences} 221, 2013, pp. 170-180.
#'
#' Gagolewski M., Grzegorzewski P., Possibilistic Analysis of Arity-Monotonic
#'    Aggregation Operators and Its Relation to Bibliometric Impact Assessment
#'    of Individuals, \emph{International Journal of Approximate Reasoning}
#'    52(9), 2011, pp. 1312-1324.
#'
#' Gagolewski M., Mesiar R., Aggregating Different Paper Quality Measures
#'    with a Generalized h-index,
#'    \emph{Journal of Informetrics} 6(4), 2012, pp. 566-579.
#'
#' Gagolewski M., Mesiar R., Monotone measures and universal integrals in
#' a uniform framework for the scientific impact assessment problem,
#'    \emph{Information Sciences} 263, 2014, pp. 166-174.
#'
#' Gagolewski M., Bibliometric Impact Assessment with R and the CITAN Package,
#'    \emph{Journal of Informetrics} 5(4), 2011, pp. 678-692.
#'
#' Gagolewski M., Grzegorzewski P., A Geometric Approach to the Construction
#'    of Scientific Impact Indices, \emph{Scientometrics} 81(3), 2009, pp. 617-634.
#'
#' Gagolewski M., \emph{Statistical Hypothesis Test for the Difference between
#'    Hirsch Indices of Two Pareto-Distributed Random Samples},
#'    In: Kruse R. et al (Eds.), \emph{Synergies of Soft Computing and Statistics
#'    for Intelligent Data Analysis} (AISC 190),
#'    Springer-Verlag, Heidelberg, 2013, pp. 359-367.
#'
#' Gagolewski M., \emph{On the Relation Between Effort-Dominating and Symmetric
#'    Minitive Aggregation Operators}, In: Greco S. et al (Eds.),
#'    \emph{Advances in Computational Intelligence}, Part III (CCIS 299),
#'    Springer-Verlag, Heidelberg, 2012, pp. 276-285.
#'
#' Gagolewski M., Debski M., Nowakiewicz M.,
#'    \emph{Efficient Algorithm for Computing Certain Graph-Based Monotone
#'    Integrals: the lp-Indices}, In: Mesiar R., Bacigal T. (Eds.),
#'    \emph{Proc. Uncertainty Modelling}, STU Bratislava,
#'    ISBN:978-80-227-4067-8, 2013, pp. 17-23.
#'
#' Gagolewski M., Grzegorzewski P., \emph{Axiomatic Characterizations
#'    of (quasi-) L-statistics and S-statistics and the Producer Assessment
#'    Problem}, In: Galichet S., Montero J., Mauris G. (Eds.),
#'    \emph{Proc. EUSFLAT/LFA 2011},
#'    Atlantic Press, 2011, pp. 53-58.
#'
#' Gagolewski M., Grzegorzewski P., \emph{S-Statistics and Their Basic Properties},
#'    In: Borgelt C. et al (Eds.), \emph{Combining Soft Computing and Statistical
#'    Methods in Data Analysis} (AISC 77), Springer-Verlag,
#'    Heidelberg, 2010, pp. 281-288.
#'
#' Gagolewski M., Grzegorzewski P., \emph{Arity-Monotonic Extended Aggregation Operators,}
#'    In: Hullermeier E., Kruse R., Hoffmann F. (Eds.),
#'    \emph{Information Processing and Management of Uncertainty in Knowledge-Based
#'    Systems} (CCIS 80), Springer-Verlag, Heidelberg, 2010, pp. 693-702.
#'
#' Grabisch M., Marichal J.-L., Mesiar R., Pap E., \emph{Aggregation functions},
#'    Cambridge University Press, 2009.
#'
#' Hirsch J.E., An index to quantify individual's scientific research output,
#'    \emph{Proceedings of the National Academy of Sciences} 102(46), 2005, pp. 16569-16572.
#'
#' Klir G.J, Yuan B., \emph{Fuzzy sets and fuzzy logic. Theory and applications},
#' Prentice Hall PTR, New Jersey, 1995.
#'
#' Kosmulski M., MAXPROD - A new index for assessment of the scientific output
#'    of an individual, and a comparison with the h-index,
#'    \emph{Cybermetrics} 11(1), 2007.
#'
#' Shilkret, N., Maxitive measure and integration,
#'    \emph{Indag. Math.} 33, 1971, pp. 109-116.
#'
#' Sugeno M., \emph{Theory of fuzzy integrals and its applications},
#'    PhD thesis, Tokyo Institute of Technology, 1974.
#'
#' Torra V., Narukawa Y., The h-index and the number of citations: Two fuzzy
#'    integrals, \emph{IEEE Transactions on Fuzzy Systems} 16(3), 2008, pp. 795-797.
#'
#' Woeginger G. J., An axiomatic characterization of the Hirsch-index.
#'    \emph{Mathematical Social Sciences} 56(2), 2008, pp. 224-232.
#'
#' Yager R.R., On ordered weighted averaging aggregation operators
#'    in multicriteria decision making, \emph{IEEE Transactions on Systems,
#'    Man, and Cybernetics} 18(1), 1988, pp. 183-190.
invisible(NULL)
