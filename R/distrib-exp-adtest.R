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


#' @title Anderson-Darling Test for Exponentiality
#'
#' @description
#' Performs an approximate Anderson-Darling goodness-of-fit
#' test, which verifies the null hypothesis:
#' Data follow an exponential distribution.
#'
#' @details
#' Sample size should be not less than 3. Missing values
#' are removed from \code{x} before applying the procedure.
#'
#' The p-value is approximate: its distribution
#' has been estimated by taking 2500000 MC samples.
#' For performance and space reasons,
#' the estimated distribution is recreated by a spline interpolation
#' on a fixed number of points.
#' As a result, the resulting p-value distribution might not necessarily
#' be uniform for p>>0.5.
#'
#' @param x a non-negative numeric vector of data values
#'
#' @return
#' A list of the class \code{htest} is returned,
#' just like in many other testing methods,
#' see, e.g., \code{\link{ks.test}}.
#'
#' @export
#' @seealso \code{\link{pexp}}
#'
#' @family Tests
#'
#' @references
#' Anderson T.W., Darling D.A.,
#' A Test of Goodness-of-Fit,
#' \emph{Journal of the American Statistical Association} 49,
#' 1954, pp. 765-769.
exp_test_ad <- function(x)
{
   DNAME <- deparse(substitute(x))

   x <- x[!is.na(x)]
   n <- length(x)

   n2 <- if(n < nrow(exp_test_ad_cdf)) n else nrow(exp_test_ad_cdf)
   if (any(is.na(exp_test_ad_cdf[n2,])))
      stop("Sample size too small")

   W <- .Call("exp_test_statistic", x, PACKAGE="agop")
   pv <- if (W > exp_test_ad_cdf[1, ncol(exp_test_ad_cdf)]) 1e-16 else
      1-splinefun(exp_test_ad_cdf[1,], exp_test_ad_cdf[n2,], method="monoH.FC")(W)


   res <- list(statistic=c(W=W), p.value=pv,
             method="Anderson-Darling exponentiality test",
             data.name=DNAME)
   attr(res, "class") <- "htest"
   res
}
