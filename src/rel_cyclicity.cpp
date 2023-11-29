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


/** Internal function
 *
 * Check cyclicity; based on depth-first search & topological sorting
 */
bool rel_is_cyclic(int i, int* xp, int n, int* helper) {
   if (helper[i] == 1)
      return true;

   if (helper[i] == 2)
      return false;

   helper[i] = 1;
   for (int j=0; j<n; ++j) {
      if (xp[i+j*n]) {
         if (j!=i && rel_is_cyclic(j, xp, n, helper))
            return true;
      }
   }
   helper[i] = 2;
   return false;
}


/** Check if a binary relation is cyclic
 *
 * @param x square logical matrix
 * @return logical scalar
 *
 * @version 0.2 (Marek Gagolewski)
 */
SEXP rel_is_cyclic(SEXP x)
{
   x = PROTECT(prepare_arg_logical_square_matrix(x, "R"));
   SEXP dim = Rf_getAttrib(x, R_DimSymbol);
   R_len_t n = INTEGER(dim)[0];
   int* xp = INTEGER(x);

   for (int i=0; i<n*n; ++i) {
      if (xp[i] == NA_LOGICAL) {
         UNPROTECT(1);
         return Rf_ScalarLogical(NA_LOGICAL);
      }
   }
   int* helper = new int[n];
   for (int i=0; i<n; ++i)
      helper[i] = 0;

   bool ret = false;
   int i=0;
   do {
      while (i < n) { if (helper[i] == 0) break; i++; } // get an unmarked node
      if (i == n) break;
      ret = rel_is_cyclic(i, xp, n, helper);
   } while(!ret);

   delete[] helper;
   UNPROTECT(1);
   return Rf_ScalarLogical(ret);
}
