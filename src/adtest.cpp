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


/** Compute the test statistics for AD exponentiality test
 *
 *
 * @param x numeric vector
 * @return real scalar (vector of length == 1)
 */
SEXP exp_test_statistic(SEXP x)
{
   x = PROTECT(prepare_arg_numeric_sorted_inc(x, "x"));

   R_len_t n = LENGTH(x);
   if (n < 3) {
      UNPROTECT(1);
      return Rf_ScalarReal(NA_REAL);
   }

   double* xd = REAL(x);

   double mean = 0.0;
   for (int i=0; i<n; i++) {
      if (xd[i] <= 0.0) {
         UNPROTECT(1);
         return Rf_ScalarReal(1.0/0.0);
      }
      mean += xd[i];
   }
   mean /= (double)n;

   double w_pom = 0.0;
   for (int i=0; i<n; i++)
      w_pom += (2.0*i+1.0)*(log(1.0-exp(-xd[i]/mean))-xd[n-i-1]/mean);

   UNPROTECT(1);
   return Rf_ScalarReal(-(double)n-w_pom/(double)n);
}
