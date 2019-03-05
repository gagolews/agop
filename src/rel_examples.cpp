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


/** Weak Dominance relation for impact functions
 *
 *
 * @param x numeric vector
 * @param y numeric vector
 * @return logical scalar, whether x <= y
 *
 * @version 0.1 (Marek Gagolewski)
 */
SEXP pord_weakdom(SEXP x, SEXP y)
{
   x = prepare_arg_numeric_sorted_dec(x, "x");
   y = prepare_arg_numeric_sorted_dec(y, "y");

   R_len_t nx = LENGTH(x);
   R_len_t ny = LENGTH(y);

   double* xd = REAL(x);
   double* yd = REAL(y);

   if (ny <= 0) Rf_error(MSG_ARG_TOO_SHORT, "x");
   if (ny <= 0) Rf_error(MSG_ARG_TOO_SHORT, "y");
   if (ISNA(xd[0])) return Rf_ScalarLogical(NA_LOGICAL);
   if (ISNA(yd[0])) return Rf_ScalarLogical(NA_LOGICAL);

//   if (xd[nx-1] < 0) Rf_error(MSG__ARG_NOT_GE_A, "x", 0.0);
//   if (yd[ny-1] < 0) Rf_error(MSG__ARG_NOT_GE_A, "y", 0.0);

   if (ny < nx)
      Rf_ScalarLogical(FALSE); // x is definitely not dominated by y

   for (R_len_t i=0; i<nx; ++i) // nx <= ny
      if (xd[i] > yd[i])
         return Rf_ScalarLogical(FALSE);

   return Rf_ScalarLogical(TRUE);
}



/** Weak Dominance relation
 *
 * @param x numeric vector
 * @param y numeric vector
 * @param incompatible_lengths single logical value
 *
 * @return logical scalar, whether x <= y
 *
 * @version 0.2-1 (Marek Gagolewski, 2014-11-19)
 */
SEXP pord_nd(SEXP x, SEXP y, SEXP incompatible_lengths)
{
   x = prepare_arg_numeric(x, "x");
   y = prepare_arg_numeric(y, "y");
   incompatible_lengths = prepare_arg_logical_1(incompatible_lengths, "incompatible_lengths");

   R_len_t nx = LENGTH(x);
   R_len_t ny = LENGTH(y);

   if (ny != nx)
      return incompatible_lengths;

   double* xd = REAL(x);
   double* yd = REAL(y);

   if (ny <= 0) Rf_error(MSG_ARG_TOO_SHORT, "x");
   if (ny <= 0) Rf_error(MSG_ARG_TOO_SHORT, "y");

//   if (xd[nx-1] < 0) Rf_error(MSG__ARG_NOT_GE_A, "x", 0.0);
//   if (yd[ny-1] < 0) Rf_error(MSG__ARG_NOT_GE_A, "y", 0.0);

   for (R_len_t i=0; i<nx; ++i) { // nx <= ny
      if (ISNA(xd[i]) || ISNA(yd[i]))
         return Rf_ScalarLogical(NA_LOGICAL);
      else if (xd[i] > yd[i])
         return Rf_ScalarLogical(FALSE);
   }

   return Rf_ScalarLogical(TRUE);
}


/** Compare vectors' spread (dispersion operators)
 *
 * @param x numeric vector
 * @param y numeric vector
 * @param incompatible_lengths single logical value
 * @return logical scalar, whether x <= y
 *
 * @version 0.2-1 (Marek Gagolewski)
 * @version 0.2-1 (Marek Gagolewski, 2014-11-19)
 */
SEXP pord_spread(SEXP x, SEXP y, SEXP incompatible_lengths)
{
   x = prepare_arg_numeric(x, "x");
   y = prepare_arg_numeric(y, "y");
   incompatible_lengths = prepare_arg_logical_1(incompatible_lengths, "incompatible_lengths");

   R_len_t nx = LENGTH(x);
   R_len_t ny = LENGTH(y);
   if (nx != ny)
      return incompatible_lengths;

   double* xd = REAL(x);
   double* yd = REAL(y);

   if (ny <= 0) Rf_error(MSG_ARG_TOO_SHORT, "x");
   if (ny <= 0) Rf_error(MSG_ARG_TOO_SHORT, "y");

   // TO DO: implement a nlogn algorithm
   // find an ordering permutation o of y
   // do check if diff(x[o]) <= diff(y[o])

   for (R_len_t j=0; j<nx; ++j) {
      for (R_len_t i=0; i<nx; ++i) {
         if (ISNA(xd[i]) || ISNA(yd[i]))
            return Rf_ScalarLogical(NA_LOGICAL);
         if (xd[i] > xd[j] && (yd[i] <= yd[j] || yd[i] - yd[j] < xd[i] - xd[j]))
            return Rf_ScalarLogical(FALSE);
      }
   }

   return Rf_ScalarLogical(TRUE);
}
