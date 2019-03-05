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
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the            *
 *   GNU Lesser General Public License for more details.                     *
 *                                                                           *
 *   You should have received a copy of the GNU Lesser General Public        *
 *   License along with 'agop'. If not, see <http://www.gnu.org/licenses/>.  *
 * ************************************************************************* */



#include "agop.h"





/** OWA operator
 *
 * @param x numeric
 * @param w numeric
 * @return numeric of length 1
 */
SEXP owa(SEXP x, SEXP w)
{
   x = prepare_arg_numeric_sorted_inc(x, "x");
   return wam(x, w);
}



/** OWMax operator
 *
 * @param x numeric
 * @param w numeric
 * @return numeric of length 1
 */
SEXP owmax(SEXP x, SEXP w)
{
   x = prepare_arg_numeric_sorted_inc(x, "x");
   return wmax(x, w);
}




/** OWMin operator
 *
 * @param x numeric
 * @param w numeric
 * @return numeric of length 1
 */
SEXP owmin(SEXP x, SEXP w)
{
   x = prepare_arg_numeric_sorted_inc(x, "x");
   return wmin(x, w);
}




/** WAM operator
 *
 * @param x numeric
 * @param w numeric
 * @return numeric of length 1
 */
SEXP wam(SEXP x, SEXP w)
{
   x = prepare_arg_numeric(x, "x");
   w = prepare_arg_numeric(w, "w");

   R_len_t x_length = LENGTH(x);
   R_len_t w_length = LENGTH(w);
   double* w_tab = REAL(w);
   double* x_tab = REAL(x);

   if (w_length <= 0) Rf_error(MSG_ARG_TOO_SHORT, "w");
   if (x_length <= 0) Rf_error(MSG_ARG_TOO_SHORT, "x");

   if (ISNA(w_tab[0]) || ISNA(x_tab[0]))
      return Rf_ScalarReal(NA_REAL);
   if (x_length != w_length)
      Rf_error(MSG__ARGS_EXPECTED_EQUAL_SIZE, "x", "w");


   double w_sum = 0.0;
   double ret_val = 0.0;
   for (R_len_t i=0; i<x_length; ++i) {
      if (w_tab[i] < 0)
         Rf_error(MSG__ARG_NOT_GE_A, "w", 0.0);
      w_sum = w_sum + w_tab[i];
      ret_val += w_tab[i]*x_tab[i];
   }

   if (w_sum > 1.0+EPS || w_sum < 1.0-EPS)
      Rf_warning("elements of `w` does not sum up to 1. correcting.");

   ret_val /= w_sum;
   return Rf_ScalarReal(ret_val);
}



/** WMax operator
 *
 * @param x numeric
 * @param w numeric
 * @return numeric of length 1
 */
SEXP wmax(SEXP x, SEXP w)
{
   x = prepare_arg_numeric(x, "x");
   w = prepare_arg_numeric(w, "w");

   R_len_t x_length = LENGTH(x);
   R_len_t w_length = LENGTH(w);
   double* w_tab = REAL(w);
   double* x_tab = REAL(x);

   if (w_length <= 0) Rf_error(MSG_ARG_TOO_SHORT, "w");
   if (x_length <= 0) Rf_error(MSG_ARG_TOO_SHORT, "x");

   if (ISNA(w_tab[0]) || ISNA(x_tab[0]))
      return Rf_ScalarReal(NA_REAL);
   if (x_length != w_length)
      Rf_error(MSG__ARGS_EXPECTED_EQUAL_SIZE, "x", "w");

   double ret_val = DBL_MIN;
   for (R_len_t i=0; i<x_length; ++i) {
      double tmp = min(w_tab[i], x_tab[i]);
      if (ret_val < tmp) ret_val = tmp;
   }

   return Rf_ScalarReal(ret_val);
}



/** WMin operator
 *
 * @param x numeric
 * @param w numeric
 * @return numeric of length 1
 */
SEXP wmin(SEXP x, SEXP w)
{
   x = prepare_arg_numeric(x, "x");
   w = prepare_arg_numeric(w, "w");

   R_len_t x_length = LENGTH(x);
   R_len_t w_length = LENGTH(w);
   double* w_tab = REAL(w);
   double* x_tab = REAL(x);

   if (w_length <= 0) Rf_error(MSG_ARG_TOO_SHORT, "w");
   if (x_length <= 0) Rf_error(MSG_ARG_TOO_SHORT, "x");

   if (ISNA(w_tab[0]) || ISNA(x_tab[0]))
      return Rf_ScalarReal(NA_REAL);
   if (x_length != w_length)
      Rf_error(MSG__ARGS_EXPECTED_EQUAL_SIZE, "x", "w");

   double ret_val = DBL_MAX;
   for (R_len_t i=0; i<x_length; ++i) {
      double tmp = max(w_tab[i], x_tab[i]);
      if (ret_val > tmp) ret_val = tmp;
   }

   return Rf_ScalarReal(ret_val);
}


///** Function to compute the S-statistic for kappa=id, O(log n) time.
// *  @param x vector of numbers, 0<=x[i]<=1, sorted non-increasingly
// *  @param n pointer to the number of observations
// *  @param out one-dimensional array which stores the result
// */
//void Sstat2(double* x, int* n, double* out)
//{
//	double N = (double)(*n);
//
//	int h1 = 0;
//	int h2 = (*n)-1;
//	int m;
//	double xmulN;
//	double mp1;
//
//	if (x[0] < 1.0/N) { *out = x[0]; return; }
//
//	while (1)
//	{
//		m = (h2+h1+1)/2;
//		mp1 = (double)(m+1);
//		xmulN = N*x[m];
//		if (xmulN == mp1 || h1 == h2) {break;}
//		if (xmulN < mp1) h2 = m-1;
//		else h1 = m;
//	}
//
//#ifdef CITAN_DEBUG
//	if (!(m+1 <= *n && m+1>=0)) fprintf(stderr, "CITAN_DEBUG: Sstat2: !(m+1 <= *n && m+1>=0)\n");
//	if (m>=0 && x[m]<mp1/N) fprintf(stderr, "CITAN_DEBUG: Sstat2: m>=0 && x[m]<mp1/N\n");
//#endif
//
//	if (m+1 < *n)
//	{
//		if (mp1 > N*x[m+1]) *out = mp1/N;
//		else                *out = x[m+1];
//	}
//	else
//	{
//		*out = mp1/N;
//	}
//}



/*
void Sstat2(double* x, int* n, double* out) -- OFTEN SLOWER THAN THE ABOVE
{
   int i = 0;
   int k = *n;
   double d = 1.0/(double)k;

   while (i<k)
   {
   	if (x[i] < d)
   	{
   		if (x[i] >= (double)i/(double)k)
   			*out = x[i];
   		else
   			*out = (double)i/(double)k;
   		return;
   	}
   	++i;
   	d = (double)(i+1)/(double)k;
   }

   // i == k
   *out = x[k-1];
}
*/
