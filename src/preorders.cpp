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
   
   if (xd[nx-1] < 0) Rf_error(MSG__ARG_NOT_GE_A, "x", 0.0);
   if (yd[ny-1] < 0) Rf_error(MSG__ARG_NOT_GE_A, "y", 0.0);
   
   if (ny < nx)
      Rf_ScalarLogical(FALSE); // x is definitely not dominated by y
   
   for (R_len_t i=0; i<nx; ++i) // nx <= ny
      if (xd[i] > yd[i])
         return Rf_ScalarLogical(FALSE);
	
   return Rf_ScalarLogical(TRUE);
}


/** Compare vectors' spread (dispersion operators)
 * 
 * @param x numeric vector
 * @param y numeric vector
 * @return logical scalar, whether x <= y
 * 
 * @version 0.1 (Marek Gagolewski)
 */
SEXP pord_spread(SEXP x, SEXP y)
{
   x = prepare_arg_numeric(x, "x");
   y = prepare_arg_numeric(y, "y");

   R_len_t nx = LENGTH(x);
   R_len_t ny = LENGTH(y);
   double* xd = REAL(x);
   double* yd = REAL(y);
   
   if (ny <= 0) Rf_error(MSG_ARG_TOO_SHORT, "x");
   if (ny <= 0) Rf_error(MSG_ARG_TOO_SHORT, "y");
   if (ISNA(xd[0])) return Rf_ScalarLogical(NA_LOGICAL);
   if (ISNA(yd[0])) return Rf_ScalarLogical(NA_LOGICAL);
   
   if (nx != ny)
      Rf_error(MSG__ARGS_EXPECTED_EQUAL_SIZE, "x", "y");
            
   for (R_len_t j=0; j<nx; ++j)
      for (R_len_t i=0; i<nx; ++i)
         if (xd[i] > xd[j] && (yd[i] <= yd[j] || yd[i] - yd[j] < xd[i] - xd[j]))
            return Rf_ScalarLogical(FALSE);
            
   return Rf_ScalarLogical(TRUE);
}


/** Compare vectors' spread (symmetric dispersion operators)
 * 
 * @param x numeric vector
 * @param y numeric vector
 * @return logical scalar, whether x <= y
 * 
 * @version 0.1 (Marek Gagolewski)
 */
SEXP pord_spreadsym(SEXP x, SEXP y)
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
   
   if (nx != ny)
      Rf_error(MSG__ARGS_EXPECTED_EQUAL_SIZE, "x", "y");
      
   for (R_len_t i=1; i<nx; ++i) {
//      cerr << xd[i-1] << " " << xd[i] << " | " << yd[i-1] << " " << yd[i] <<endl;
      if (xd[i-1] > xd[i] && (yd[i-1] <= yd[i] || yd[i-1] - yd[i] < xd[i-1] - xd[i]))
         return Rf_ScalarLogical(FALSE);
   }
   
   return Rf_ScalarLogical(TRUE);
}


/** Check if a binary relation is reflexive
 * 
 * @param x square logical matrix
 * @return logical scalar
 * 
 * @version 0.1 (Marek Gagolewski)
 */
SEXP rel_is_reflexive(SEXP x)
{
   x = prepare_arg_logical_square_matrix(x, "R");
   SEXP dim = Rf_getAttrib(x, R_DimSymbol);
   R_len_t n = INTEGER(dim)[0];
   int* xp = INTEGER(x);
   for (R_len_t i=0; i<n; ++i) {
      if (xp[i+i*n] == NA_LOGICAL)
         return Rf_ScalarLogical(NA_LOGICAL);
      else if (!xp[i+i*n])
         return Rf_ScalarLogical(FALSE);
   }
   return Rf_ScalarLogical(TRUE);
}


/** Check if a binary relation is total
 * 
 * @param x square logical matrix
 * @return logical scalar
 * 
 * @version 0.1 (Marek Gagolewski)
 */
SEXP rel_is_total(SEXP x)
{
   x = prepare_arg_logical_square_matrix(x, "R");
   SEXP dim = Rf_getAttrib(x, R_DimSymbol);
   R_len_t n = INTEGER(dim)[0];
   int* xp = INTEGER(x);
   for (R_len_t i=0; i<n; ++i) {
      for (R_len_t j=i; j<n; ++j) {
         if (( xp[i+j*n] == NA_LOGICAL &&  xp[j+i*n] == NA_LOGICAL )
          || ( xp[i+j*n] == NA_LOGICAL && !xp[j+i*n]               )
          || (!xp[i+j*n]               &&  xp[j+i*n] == NA_LOGICAL ))
            return Rf_ScalarLogical(NA_LOGICAL);
         else if (!xp[i+j*n] && !xp[j+i*n]) // NA_LOGICAL != 0
            return Rf_ScalarLogical(FALSE);
      }
   }
   return Rf_ScalarLogical(TRUE);
}



/** Check if a binary relation is transitive
 * 
 * @param x square logical matrix
 * @return logical scalar
 * 
 * @version 0.1 (Marek Gagolewski)
 */
SEXP rel_is_transitive(SEXP x)
{
   x = prepare_arg_logical_square_matrix(x, "R");
   SEXP dim = Rf_getAttrib(x, R_DimSymbol);
   R_len_t n = INTEGER(dim)[0];
   int* xp = INTEGER(x);
   for (R_len_t i=0; i<n; ++i) {
      for (R_len_t j=0; j<n; ++j) {
         if (i == j) continue; // don't care
         if (xp[i+j*n] == NA_LOGICAL)
            return Rf_ScalarLogical(NA_LOGICAL); // this could be done better
         if (!xp[i+j*n]) continue; // nothing more to check
         for (R_len_t k=0; k<n; ++k) {
            if (xp[i+k*n] == NA_LOGICAL || xp[j+k*n] == NA_LOGICAL)
               return Rf_ScalarLogical(NA_LOGICAL); // this could be done better
            if (xp[j+k*n] && !xp[i+k*n])
               return Rf_ScalarLogical(FALSE);
         }
      }
   }
   return Rf_ScalarLogical(TRUE);
}


/** Get the transitive closure of a binary relation
 * 
 * @param x square logical matrix
 * @return square logical matrix
 * 
 * @version 0.1 (Marek Gagolewski)
 */
SEXP rel_closure_transitive(SEXP x)
{
   x = prepare_arg_logical_square_matrix(x, "R");
   SEXP dim = Rf_getAttrib(x, R_DimSymbol);
   R_len_t n = INTEGER(dim)[0];
   int* xp = INTEGER(x);

   SEXP y = Rf_allocVector(LGLSXP, n*n);
   int* yp = INTEGER(y);
   Rf_setAttrib(y, R_DimSymbol, dim);

   for (R_len_t i=0; i<n*n; ++i) {
      if (xp[i] == NA_LOGICAL)
         Rf_error(MSG__ARG_EXPECTED_NOT_NA, "R"); // missing values are not allowed
      yp[i] = xp[i];
   }
   
   for (R_len_t k=0; k<n; ++k) { // Warshall's algorithm
      for (R_len_t i=0; i<n; ++i) {
         for (R_len_t j=0; j<n; ++j) {
            yp[i+n*j] = (yp[i+n*j] || (yp[i+n*k] && yp[k+n*j]));
         }
      }
   }
   
   return y;
}



/** Get the reflexive closure of a binary relation
 * 
 * @param x square logical matrix
 * @return square logical matrix
 * 
 * @version 0.1 (Marek Gagolewski)
 */
SEXP rel_closure_reflexive(SEXP x)
{
   x = prepare_arg_logical_square_matrix(x, "R");
   SEXP dim = Rf_getAttrib(x, R_DimSymbol);
   R_len_t n = INTEGER(dim)[0];
   int* xp = INTEGER(x);

   SEXP y = Rf_allocVector(LGLSXP, n*n);
   int* yp = INTEGER(y);
   Rf_setAttrib(y, R_DimSymbol, dim);

   for (R_len_t i=0; i<n*n; ++i) {
      yp[i] = xp[i];
   }
   
   for (R_len_t i=0; i<n; ++i) 
      yp[i+n*i] = TRUE;
   
   return y;
}


/** Get the fair totalization of a binary relation
 * 
 * @param x square logical matrix
 * @return square logical matrix
 * 
 * @version 0.1 (Marek Gagolewski)
 */
SEXP rel_closure_total_fair(SEXP x)
{
   x = prepare_arg_logical_square_matrix(x, "R");
   SEXP dim = Rf_getAttrib(x, R_DimSymbol);
   R_len_t n = INTEGER(dim)[0];
   int* xp = INTEGER(x);

   SEXP y = Rf_allocVector(LGLSXP, n*n);
   int* yp = INTEGER(y);
   Rf_setAttrib(y, R_DimSymbol, dim);

   for (R_len_t i=0; i<n*n; ++i) {
      if (xp[i] == NA_LOGICAL)
         Rf_error(MSG__ARG_EXPECTED_NOT_NA, "R"); // missing values are not allowed
      yp[i] = xp[i];
   }
   
   for (R_len_t i=0; i<n; ++i) {
      for (R_len_t j=i; j<n; ++j) {
         if (!yp[i+n*j] && !yp[j+n*i]) {
            yp[i+n*j] = TRUE;
            yp[j+n*i] = TRUE;
         }
      }
   }
   
   return y;
}

