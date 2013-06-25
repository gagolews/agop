/* ************************************************************************* *
 *   This file is part of the `agop` library.                                *
 *                                                                           *
 *   Copyright 2013 Marek Gagolewski, Anna Cena                              *                                                                    *
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


/** Weak Dominance relation for impact functions
 * 
 * 
 * @param x numeric vector
 * @param y numeric vector
 * @return logical scalar, whether x <= y
 */
SEXP pord_weakdom(SEXP x, SEXP y)
{
   x = prepare_arg_numeric_sorted_0_infty(x, "x");
   R_len_t nx = LENGTH(x);
   if (nx <= 0)
      return ScalarLogical(TRUE); // x empty - y whatever
   double* xd = REAL(x);
   if (R_IsNA(xd[0]))
      return ScalarLogical(NA_LOGICAL);
      
   y = prepare_arg_numeric_sorted_0_infty(y, "y");
   R_len_t ny = LENGTH(y);
   if (ny <= 0) 
      return ScalarLogical(FALSE); // x is not empty
   double* yd = REAL(y);
   if (R_IsNA(yd[0]))
      return ScalarLogical(NA_LOGICAL);
      
   if (ny < nx)
      ScalarLogical(FALSE); // x is definitely not dominated by y
   
   for (R_len_t i=0; i<nx; ++i) // nx <= ny
      if (xd[i] > yd[i])
         return ScalarLogical(FALSE);
	
   return ScalarLogical(TRUE);
}
