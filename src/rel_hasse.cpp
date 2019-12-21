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


/** Get the reflexive and transitive reduction of a binary relation;
 *  useful for drawing Hasse diagrams
 *
 * @param x square logical matrix
 * @return square logical matrix
 *
 * @version 0.2-1 (Marek Gagolewski)
 */
SEXP rel_reduction_hasse(SEXP x)
{
   x = PROTECT(rel_closure_transitive(x));
   // is logical matrix, dimnames are preserved, no NAs, we may overwrite its elements

   SEXP dim = Rf_getAttrib(x, R_DimSymbol);
   R_len_t n = INTEGER(dim)[0];
   int* xp = INTEGER(x);

   SEXP y = PROTECT(Rf_allocVector(LGLSXP, n*n));
   int* yp = INTEGER(y);
   Rf_setAttrib(y, R_DimSymbol, dim);
   Rf_setAttrib(y, R_DimNamesSymbol, Rf_getAttrib(x, R_DimNamesSymbol)); // preserve dimnames

   for (R_len_t i=0; i<n; ++i) {
      for (R_len_t j=0; j<n; ++j) {
         if (i == j)
            yp[i+n*j] = 0; // remove loop
         else {
            yp[i+n*j] = xp[i+n*j];
            if (xp[i+n*j] && !xp[j+n*i]) {
               // determine whether i -> j may be removed
               for (R_len_t k=0; k<n; ++k) {
                  if (i != k && k != j && xp[i+n*k] && xp[k+n*j] && !xp[k+n*i] && !xp[j+n*k]) {
                     yp[i+n*j] = 0;
                     break;
                  }
               }
            }
         }
      }
   }

   UNPROTECT(2);
   return y;
}
