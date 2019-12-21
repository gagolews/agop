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



/** Check weights for the D2OWA operator
 *
 * @param w numeric
 * @return scalar logical
 */
SEXP d2owa_checkwts(SEXP w) {
   w = PROTECT(prepare_arg_numeric(w, "w"));
   R_len_t wn = LENGTH(w);
   double* wd = REAL(w);

   if (wn <= 1) Rf_error(MSG_ARG_TOO_SHORT, "w");
   if (ISNA(wd[0])) {
      UNPROTECT(1);
      return Rf_ScalarLogical(NA_LOGICAL);
   }

   double ws = 0.0;
   for (R_len_t i=0; i<wn; ++i) {
      if (wd[i] < 0)
         Rf_error(MSG__ARG_NOT_GE_A, "w", 0.0);
      ws += wd[i];
   }

   vector<double> zeta(wn-1);
   zeta[wn-2] = wd[wn-1]/ws;
   for (int i=wn-3; i>=0; --i)
      zeta[i] = zeta[i+1] + wd[i+1]/ws;

   for (int p=1; p<=wn-2; ++p) {
      for (int i=wn-1; i>p; --i) {
         if ((i-p)*(i-p) < 4*p*(wn-i))
            break;
         if ((wn-i)*(1.0-zeta[p-1]) + (wn*zeta[p-1]-wn+p)*zeta[i-1] < 0.0) {
            UNPROTECT(1);
            return Rf_ScalarLogical(FALSE);
         }
      }
   }

   UNPROTECT(1);
   return Rf_ScalarLogical(TRUE);
}
