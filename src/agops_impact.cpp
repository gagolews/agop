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


/** Compute the h-index, O(n) time for sorted data
 *
 * A simple binsearch-based algorithm could be implemented here,
 * but after some testing it turned to be slower for vectors of length < 1000
 * (a typical case in bibliometrics)
 *
 * @param x numeric vector
 * @return real scalar (vector of length == 1)
 */
SEXP index_h(SEXP x)
{
   x = PROTECT(prepare_arg_numeric_sorted_dec(x, "x"));

   R_len_t n = LENGTH(x);
   if (n <= 0) Rf_error(MSG_ARG_TOO_SHORT, "x");

   double* xd = REAL(x);
   if (ISNA(xd[0])) {
      UNPROTECT(1);
      return Rf_ScalarReal(NA_REAL);
   }

   if (xd[n-1] < 0) Rf_error(MSG__ARG_NOT_GE_A, "x", 0.0);

   R_len_t i = 0;
   while (i < n)	{
   	if (xd[i] < (double)i+1) break;
   	++i;
   }

   UNPROTECT(1);
   return Rf_ScalarReal((double) i);
}






/** Function to compute the g-index
 *
 *  @param x vector of non-negative reals, sorted non-increasingly
 *  @return scalar real
 */
SEXP index_g(SEXP x)
{
   x = PROTECT(prepare_arg_numeric_sorted_dec(x, "x"));

   R_len_t n = LENGTH(x);
   if (n <= 0) Rf_error(MSG_ARG_TOO_SHORT, "x");

   double* xd = REAL(x);
   if (ISNA(xd[0])) {
      UNPROTECT(1);
      return Rf_ScalarReal(NA_REAL);
   }

   if (xd[n-1] < 0) Rf_error(MSG__ARG_NOT_GE_A, "x", 0.0);

   double sum = 0.0;
   R_len_t i = 0;
   while (i < n)	{
   	sum += xd[i];
   	if (sum < (double)(i+1)*(double)(i+1)) break;
   	++i;
   }

   UNPROTECT(1);
   return Rf_ScalarReal((double) i);
}




/** Function to compute the ZERO-INSENSITIVE g-index
 *
 *  @param x vector of non-negative reals, sorted non-increasingly
 *  @return scalar real
 */
SEXP index_g_zi(SEXP x)
{
   x = PROTECT(prepare_arg_numeric_sorted_dec(x, "x"));

   R_len_t n = LENGTH(x);
   if (n <= 0) Rf_error(MSG_ARG_TOO_SHORT, "x");

   double* xd = REAL(x);
   if (ISNA(xd[0])) {
      UNPROTECT(1);
      return Rf_ScalarReal(NA_REAL);
   }

   if (xd[n-1] < 0) Rf_error(MSG__ARG_NOT_GE_A, "x", 0.0);

   double sum = 0.0;
   R_len_t i = 0;
   while (TRUE)   {
   	if (i < n) sum += xd[i];
      if (sum < (double)(i+1)*(double)(i+1)) break;
   	++i;
   }

   UNPROTECT(1);
   return Rf_ScalarReal((double) i);
}





/** Function to compute the w-index
 *
 *  @param x vector of non-negative reals, sorted non-increasingly
 *  @return scalar real
 */
SEXP index_w(SEXP x)
{
   x = PROTECT(prepare_arg_numeric_sorted_dec(x, "x"));

   R_len_t n = LENGTH(x);
   if (n <= 0) Rf_error(MSG_ARG_TOO_SHORT, "x");

   double* xd = REAL(x);
   if (ISNA(xd[0])) {
      UNPROTECT(1);
      return Rf_ScalarReal(NA_REAL);
   }

   if (xd[n-1] < 0) Rf_error(MSG__ARG_NOT_GE_A, "x", 0.0);

   R_len_t w = min(xd[0],(double)n);
   for (R_len_t i=1; i < n; ++i) {
      if (xd[i] < w-i) {
         w = (R_len_t)(xd[i]+i);
      }
      if (xd[i] == 0) {
         w = min(w,i+1);
         break;
      }
   }

   UNPROTECT(1);
   return Rf_ScalarReal((double) w);
}




/** Compute the MAXPROD-index (Kosmulski)
 *
 *
 * @param x numeric vector
 * @return real scalar
 */
SEXP index_maxprod(SEXP x)
{
   x = PROTECT(prepare_arg_numeric_sorted_dec(x, "x"));

   R_len_t n = LENGTH(x);
   if (n <= 0) Rf_error(MSG_ARG_TOO_SHORT, "x");

   double* xd = REAL(x);
   if (ISNA(xd[0])) {
      UNPROTECT(1);
      return Rf_ScalarReal(NA_REAL);
   }

   if (xd[n-1] < 0) Rf_error(MSG__ARG_NOT_GE_A, "x", 0.0);

   double out = 0.0;
   for (R_len_t i = 0; i < n && xd[i] > 0; ++i)
      if (out < xd[i]*(double)(i+1))
         out = xd[i]*(double)(i+1);

   UNPROTECT(1);
   return Rf_ScalarReal(out);
}



///** *internal* function to compute the h-index, O(log n) time for sorted vectors.
// * This has been described in (Gagolewski, Grzegorzewski, 2009).
// * For "real-world" data (short vectors), this function is slower than the O(n)
// * version.
// *
// *  @param x vector of non-negative reals, sorted non-increasingly
// *  @param n number of observations, n >= 1
// *  @return result
// */
//int __index_h_log(double* x, int n)
//{
//   int h1 = 0;
//	int h2 = n-1;
//
//	if (x[0] < 1.0) { return 0; }
//
//	while (1)
//	{
//		int m = (h2+h1+1)/2;
//		if (x[m] == (double)m+1 || h1 == h2) {return m+1; }
//		if (x[m] < (double)m+1) h2 = m-1;
//		else h1 = m;
//	}
//}




/** Function to compute the r_p-index
 *
 *  @param x numeric
 *  @param p numeric, >=1, length 1
 *  @param single numeric
 */
SEXP index_rp(SEXP x, SEXP p)
{
   p = PROTECT(prepare_arg_numeric(p, "p"));
   if (LENGTH(p) != 1)
      Rf_error("`p` should be a single numeric value");
   double p_val = REAL(p)[0];
   if (ISNA(p_val) || p_val < 1)
      Rf_error("`p` should be >= 1");

   x = PROTECT(prepare_arg_numeric_sorted_dec(x, "x"));
   R_len_t n = LENGTH(x);
   if (n <= 0) Rf_error(MSG_ARG_TOO_SHORT, "x");

   double* xd = REAL(x);
   if (ISNA(xd[0])) {
      UNPROTECT(2);
      return Rf_ScalarReal(NA_REAL);
   }

   if (xd[n-1] < 0) Rf_error(MSG__ARG_NOT_GE_A, "x", 0.0);

   if (!R_FINITE(p_val))
   {
      // this is OWMax for w=1,2,3,....
      double ret_val = DBL_MIN;
      for (R_len_t i=0; i<n; ++i) {
         double tmp = min((double)(i+1), xd[i]);
         if (ret_val < tmp) ret_val = tmp;
      }
      UNPROTECT(2);
      return Rf_ScalarReal(ret_val);
   }
   else {
      if (p_val > 50)
         Rf_warning("p is large but finite. possible accuracy problems.");

      double r2p = pow((double)n, p_val);
      R_len_t i;

   	for (i=0; i<n; ++i)
   	{
   		double xip = pow(xd[i], p_val);
   		double ip  = pow((double)i, p_val);
   		if (r2p-ip > xip)
   			r2p = ip + xip;
   	}

   	UNPROTECT(2);
   	return Rf_ScalarReal(pow(r2p, 1.0/p_val));
   }
}





/** internal **/
double2 __index_lp_finite_getAB(double p, double ui, double vi, double uj, double vj)
{
   double uip = pow(ui,p);
   double ujp = pow(uj,p);
   double vip = pow(vi,p);
   double vjp = pow(vj,p);
   double c = uip*vjp-ujp*vip;
   return double2(c/(vjp-vip), -c/(ujp-uip));
}


/** internal **/
bool __index_lp_finite_testContains(double uk, double vk, double p, double ui, double vi, double uj, double vj)
{
   // check if L^p ellipse interpolating (ui,vi) and (uj,vj) contains (uk,vk)
   double2 ab = __index_lp_finite_getAB(p,ui,vi,uj,vj);
   return (ab.v2*(1.0 - pow(uk,p)/ab.v1) >= pow(vk,p));
}


/** Function to compute the l_p-index
 *
 *  @param x numeric
 *  @param p numeric, >=1, length 1
 *  @param  numeric vector of length 2
 */
SEXP index_lp(SEXP x, SEXP p)
{
   p = PROTECT(prepare_arg_numeric(p, "p"));
   if (LENGTH(p) != 1)
      Rf_error("`p` should be a single numeric value");
   double p_val = REAL(p)[0];
   if (ISNA(p_val) || p_val < 1)
      Rf_error("`p` should be >= 1");

   x = PROTECT(prepare_arg_numeric_sorted_dec(x, "x"));
   R_len_t n = LENGTH(x);
   if (n <= 0) Rf_error(MSG_ARG_TOO_SHORT, "x");


   double* xd = REAL(x);
   if (ISNA(xd[0])) {
      UNPROTECT(2);
      return (double2(NA_REAL, NA_REAL).toR());
   }

   if (xd[n-1] < 0) Rf_error(MSG__ARG_NOT_GE_A, "x", 0.0);

   if (xd[1] <= 0.0) {
      UNPROTECT(2);
      return (double2(0.0, 0.0).toR());
   }

   if (!R_FINITE(p_val))
   {
      // this is OWMax for w=1,2,3,....
      double max_prod = 0.0;
      double2 ab;
      for (R_len_t i=0; i<n; ++i) {
         if (max_prod < (double)(i+1)*xd[i]) {
            max_prod = (double)(i+1)*xd[i];
            ab.v1 = (double)(i+1);
            ab.v2 = xd[i];
         }
      }
      UNPROTECT(2);
      return ab.toR();
   }
   else {
      if (p_val > 50)
         Rf_warning("p is large but finite. possible accuracy problems.");


// * Function to compute the l_p-index, O(n) time, p<Inf
// *  The procedure bases on Graham's scan for determining the convex hull
// *  of a planar set, see (Gagolewski,Debski,Nowakiewicz,2009b).

      deque<double2> stack;
      stack.push_back(double2(0.0, xd[0]));
      R_len_t i = 0;
      while (i<n && xd[i] >= xd[0]) ++i;
      stack.push_back(double2((double)i, xd[i]));

      for (++i; i<=n; ++i)
      {
   		double vi = (i<n)?xd[i]:0.0;
   		if (vi >= stack.back().v2) continue;

   		while (stack.size()>=2 && __index_lp_finite_testContains(
               stack.at(stack.size()-2).v1, stack.at(stack.size()-2).v2,
               p_val,
               stack.back().v1, stack.back().v2,
               (double)i, vi))
         {
   			stack.pop_back();
         }
         stack.push_back(double2((double)i, vi));
   	}

      /*  now, selectMaxPair()  */
   	double2 ab = __index_lp_finite_getAB(p_val,
         stack.at(0).v1, stack.at(0).v2,
         stack.at(1).v1, stack.at(1).v2);
   	for (i=1; i<(R_len_t)stack.size()-1; ++i)
   	{
   		double2 ab2 = __index_lp_finite_getAB(p_val,
            stack.at(i).v1, stack.at(i).v2,
            stack.at(i+1).v1, stack.at(i+1).v2);

   		if (ab.v1*ab.v2 < ab2.v1*ab2.v2)
            ab = ab2;
   	}

      UNPROTECT(2);
      return double2(pow(ab.v1, 1.0/p_val), pow(ab.v2, 1.0/p_val)).toR();
   }
}
