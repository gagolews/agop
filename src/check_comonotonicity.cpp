/* ************************************************************************* *
 *   This file is part of the `agop` library.                                *
 *                                                                           *
 *   Copyright 2013-2019 Marek Gagolewski, Anna Cena                         *
 *                                                                           *
 *   'agop' is free software: you can redistribute it and/or modify          *
 *   it under the terms of the GNU Lesser General Public License             *
 *   as published by the Free Software Foundation, either version 3          *
 *   of the License, or (at your option) any later version.                  *
 *                                                                           *
 *   'agop' is distributed in the hope that it will be useful,               *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of          *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.See the             *
 *   GNU Lesser General Public License for more details.                     *
 *                                                                           *
 *   You should have received a copy of the GNU Lesser General Public        *
 *   License along with 'agop'. If not, see <http://www.gnu.org/licenses/>.  *
 * ************************************************************************* */



#include "agop.h"


/** Check if two vectors are comonotonic
 *
 * @param x numeric vector
 * @param y numeric vector
 * @param incompatible_lengths single logical value
 * @return logical scalar
 *
 * @version 0.2-1 (Marek Gagolewski)
 *
 * @version 0.2-1 (Marek Gagolewski, 2014-11-19)
 *    incompatible_lenghts arg added
 *
 * @version 0.2-3 (Marek Gagolewski, 2019-12-21)
 *    #8: PROTECT from gc
 */
SEXP check_comonotonicity(SEXP x, SEXP y, SEXP incompatible_lengths)
{
   x = PROTECT(prepare_arg_numeric(x, "x"));
   y = PROTECT(prepare_arg_numeric(y, "y"));
   incompatible_lengths = PROTECT(prepare_arg_logical_1(incompatible_lengths, "incompatible_lengths"));

   R_len_t x_length = LENGTH(x);
   R_len_t y_length = LENGTH(y);

   if (x_length != y_length) {
      UNPROTECT(3);
      return incompatible_lengths;
   }

   double* x_tab = REAL(x);
   double* y_tab = REAL(y);

   for (R_len_t i=0; i<x_length; ++i) {
      if (ISNA(x_tab[i]) || ISNA(y_tab[i])) {
         UNPROTECT(3);
         return Rf_ScalarLogical(NA_LOGICAL);
      }

      for (R_len_t j=i; j<x_length; ++j) {
         if ((x_tab[i]-x_tab[j])*(y_tab[i]-y_tab[j]) < 0.0) {
            UNPROTECT(3);
            return Rf_ScalarLogical(FALSE);
         }
      }
   }

   UNPROTECT(3);
   return Rf_ScalarLogical(TRUE);
}
