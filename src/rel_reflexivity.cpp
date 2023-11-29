/* ************************************************************************* *
 * This file is part of the 'agop' library.                                  *
 *                                                                           *
 * Copyleft (c) 2013-2023, Marek Gagolewski <https://www.gagolewski.com/>    *
 *                                                                           *
 *                                                                           *
 * 'agop' is free software: you can redistribute it and/or modify it under   *
 * the terms of the GNU Lesser General Public License as published by        *
 * the Free Software Foundation, either version 3 of the License, or         *
 * (at your option) any later version.                                       *
 *                                                                           *
 * 'agop' is distributed in the hope that it will be useful,                 *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of            *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the              *
 * GNU Lesser General Public License for more details.                       *
 *                                                                           *
 * A copy of the GNU Lesser General Public License can be downloaded         *
 * from <http://www.gnu.org/licenses/>.                                      *
 * ************************************************************************* */



#include "agop.h"


/** Check if a binary relation is reflexive
 *
 * @param x square logical matrix
 * @return logical scalar
 *
 * @version 0.2 (Marek Gagolewski)
 */
SEXP rel_is_reflexive(SEXP x)
{
   x = PROTECT(prepare_arg_logical_square_matrix(x, "R"));
   SEXP dim = Rf_getAttrib(x, R_DimSymbol);
   R_len_t n = INTEGER(dim)[0];
   int* xp = INTEGER(x);
   for (R_len_t i=0; i<n; ++i) {
      if (xp[i+i*n] == NA_LOGICAL) {
         UNPROTECT(1);
         return Rf_ScalarLogical(NA_LOGICAL);
      }
      else if (!xp[i+i*n]) {
         UNPROTECT(1);
         return Rf_ScalarLogical(FALSE);
      }
   }
   UNPROTECT(1);
   return Rf_ScalarLogical(TRUE);
}



/** Get the reflexive closure of a binary relation
 *
 * @param x square logical matrix
 * @return square logical matrix
 *
 * @version 0.2 (Marek Gagolewski)
 */
SEXP rel_closure_reflexive(SEXP x)
{
   x = PROTECT(prepare_arg_logical_square_matrix(x, "R"));
   SEXP dim = Rf_getAttrib(x, R_DimSymbol);
   R_len_t n = INTEGER(dim)[0];
   int* xp = INTEGER(x);

   SEXP y = PROTECT(Rf_allocVector(LGLSXP, n*n));
   int* yp = INTEGER(y);
   Rf_setAttrib(y, R_DimSymbol, dim);
   Rf_setAttrib(y, R_DimNamesSymbol, Rf_getAttrib(x, R_DimNamesSymbol)); // preserve dimnames

   for (R_len_t i=0; i<n*n; ++i) {
      yp[i] = xp[i];
   }

   for (R_len_t i=0; i<n; ++i)
      yp[i+n*i] = TRUE;

   UNPROTECT(2);
   return y;
}


/** Get the reflexive reduction of a binary relation
 *
 * @param x square logical matrix
 * @return square logical matrix
 *
 * @version 0.2 (Marek Gagolewski)
 */
SEXP rel_reduction_reflexive(SEXP x)
{
   x = PROTECT(prepare_arg_logical_square_matrix(x, "R"));
   SEXP dim = Rf_getAttrib(x, R_DimSymbol);
   R_len_t n = INTEGER(dim)[0];
   int* xp = INTEGER(x);

   SEXP y = PROTECT(Rf_allocVector(LGLSXP, n*n));
   int* yp = INTEGER(y);
   Rf_setAttrib(y, R_DimSymbol, dim);
   Rf_setAttrib(y, R_DimNamesSymbol, Rf_getAttrib(x, R_DimNamesSymbol)); // preserve dimnames

   for (R_len_t i=0; i<n*n; ++i) {
      yp[i] = xp[i];
   }

   for (R_len_t i=0; i<n; ++i)
      yp[i+n*i] = FALSE;

   UNPROTECT(2);
   return y;
}
