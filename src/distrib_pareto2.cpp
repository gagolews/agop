/* ************************************************************************* *
 *   This file is part of the `agop` library.                                *
 *                                                                           *
 *   Copyright 2013-2019 Marek Gagolewski, Anna Cena                         *
 *                                                                           *
 *   Parts of the code are taken from the 'CITAN' R package by M. Gagolewski *
 *                                                                           *
 *   'agop' is free software: you can redistribute it and/or modify          *
 *   it under the terms of the GNU Lesser General Public License             *
 *   as published by the Free Software Foundation, either version 3          *
 *   of the License, or (at your option) any later version.                  *
 *                                                                           *
 *   'agop' is distributed in the hope that it will be useful,               *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of          *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the            *
 *   GNU Lesser General Public License for more details.                     *
 *                                                                           *
 *   You should have received a copy of the GNU Lesser General Public        *
 *   License along with 'agop'. If not, see <http://www.gnu.org/licenses/>.  *
 * ************************************************************************* */



#include "agop.h"


/** Compute the c.d.f. of a Pareto2 distribution
 *
 * 10x+ faster than the original version:
 * ppareto2_old <- function(q, k=1, s=1, lower.tail=TRUE)
 * {
 *    stopifnot(is.numeric(k), k > 0)
 *    stopifnot(is.numeric(s), s > 0)
 *    stopifnot(is.numeric(q))
 *    ret <- ifelse(q<0, 0, (1-(s/(s+q))^k))
 *    if (identical(lower.tail[1], FALSE))
 *       ret <- 1-ret
 *    else
 *       ret
 * }
 *
 * @param q numeric vector
 * @param k numeric vector
 * @param s numeric vector
 * @param lower_tail single logical value
 * @return real scalar (vector of length == 1)
 */
SEXP ppareto2(SEXP q, SEXP k, SEXP s, SEXP lower_tail)
{
   q = PROTECT(prepare_arg_double(q, "q"));
   k = PROTECT(prepare_arg_double(k, "k"));
   s = PROTECT(prepare_arg_double(s, "s"));
   lower_tail = PROTECT(prepare_arg_logical_1(lower_tail, "lower.tail"));

   R_len_t nq = LENGTH(q);
   R_len_t nk = LENGTH(k);
   R_len_t ns = LENGTH(s);
   if (min(min(nq, nk), ns) <= 0) {
      UNPROTECT(4);
      return Rf_allocVector(REALSXP, 0);
   }
   int*    ptail = INTEGER(lower_tail);
   if (ptail[0] == NA_LOGICAL)
      Rf_error(MSG__ARG_EXPECTED_NOT_NA, "lower.tail");

   double* pq = REAL(q);
   double* pk = REAL(k);
   double* ps = REAL(s);
   R_len_t n = max(max(nq, nk), ns);
   if (n%nq != 0) Rf_warning(MSG__WARN_RECYCLE);
   if (n%nk != 0) Rf_warning(MSG__WARN_RECYCLE);
   if (n%ns != 0) Rf_warning(MSG__WARN_RECYCLE);



   if (nk == 1 && ns == 1) { // the most typical case
      double vs = ps[0];
      double vk = pk[0];
      if (ISNA(vs) || ISNA(vk)) {
         UNPROTECT(4);
         return vector_NA_double(n);
      }

      if (vs <= 0) Rf_error(MSG__ARG_NOT_GT_A, "s", 0);
      if (vk <= 0) Rf_error(MSG__ARG_NOT_GT_A, "k", 0);

      SEXP ret;
      PROTECT(ret = Rf_allocVector(REALSXP, n));
      double* pret = REAL(ret);
      if ((bool)ptail[0]) {
         for (R_len_t i=0; i<n; i++) {
            pret[i] = (ISNA(pq[i]))?NA_REAL:
               ((pq[i]>0)
               ?(1.0-pow(vs/(vs+pq[i]), vk))
               :(0.0));
         }
      }
      else {
         for (R_len_t i=0; i<n; i++) {
            pret[i] = (ISNA(pq[i]))?NA_REAL:
               ((pq[i])
               ?(pow(vs/(vs+pq[i]), vk))
               :(1.0));
         }
      }

      UNPROTECT(5);
      return ret;
   }
   else {
      SEXP ret;
      PROTECT(ret = Rf_allocVector(REALSXP, n));
      double* pret = REAL(ret);

      if ((bool)ptail[0]) {
         for (R_len_t i=0; i<n; i++) {
            if (!ISNA(ps[i%ns]) && ps[i%ns] <= 0.0)
               Rf_error(MSG__ARG_NOT_GT_A, "s", 0);
            if (!ISNA(pk[i%nk]) && pk[i%nk] <= 0.0)
               Rf_error(MSG__ARG_NOT_GT_A, "k", 0);

            pret[i] = (ISNA(pq[i%nq]) || ISNA(ps[i%ns]) || ISNA(pk[i%nk]))?NA_REAL:
               ((pq[i%nq]>0)
               ?(1.0-pow(ps[i%ns]/(ps[i%ns]+pq[i%nq]), pk[i%nk]))
               :(0.0));
         }
      }
      else {
         for (R_len_t i=0; i<n; i++) {
            if (!ISNA(ps[i%ns]) && ps[i%ns] <= 0.0)
               Rf_error(MSG__ARG_NOT_GT_A, "s", 0);
            if (!ISNA(pk[i%nk]) && pk[i%nk] <= 0.0)
               Rf_error(MSG__ARG_NOT_GT_A, "k", 0);

            pret[i] = (ISNA(pq[i%nq]) || ISNA(ps[i%ns]) || ISNA(pk[i%nk]))?NA_REAL:
               ((pq[i%nq])
               ?(pow(ps[i%ns]/(ps[i%ns]+pq[i%nq]), pk[i%nk]))
               :(1.0));
         }
      }

      UNPROTECT(5);
      return ret;
   }
}
