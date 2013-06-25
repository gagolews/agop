/* ************************************************************************* *
 *   This file is part of the `agop` library.                                *
 *                                                                           *
 *   Copyright 2013 Marek Gagolewski, Anna Cena                              *                                                                 *
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



bool comparer_greater(double i, double j) { return (i>j); }




/**
 * Prepare numeric vector
 * 
 * if x is not numeric (or not coercible to), throw error.
 * if of length 0, return numeric(0).
 * if any NA, return NA_real_.
 * otherwise, return as-is
 * 
 * 
 * @param numeric vector
 * @param argname argument name (message formatting)
 * @return numeric vector
 */
SEXP prepare_arg_numeric(SEXP x, const char* argname)
{
   x = prepare_arg_double(x, argname);
   R_len_t n = LENGTH(x);
   if (n <= 0)
      return x; // empty vector => return an empty vector
      
   double* xd = REAL(x);
   for (R_len_t i=0; i<n; ++i) {
      if (R_IsNA(xd[i])) {
         return ScalarReal(NA_REAL);
      }
   }
   return x;
}


/**
 * Prepare sorted numeric vector
 * 
 * if x is not numeric (or not coercible to), throw error.
 * if of length 0, return numeric(0).
 * if any NA, return NA_real_.
 * if not sorted, return sorted.
 * otherwise, return as-is
 * 
 * 
 * @param numeric vector
 * @param argname argument name (message formatting)
 * @return numeric vector, sorted non-increasingly
 */
SEXP prepare_arg_numeric_sorted(SEXP x, const char* argname)
{
   x = prepare_arg_double(x, argname);
   R_len_t n = LENGTH(x);
   if (n <= 0)
      return x; // empty vector => return an empty vector
      
   double* xd = REAL(x);
   bool sorted = true;
   for (R_len_t i=0; i<n; ++i) {
      if (R_IsNA(xd[i])) {
         return ScalarReal(NA_REAL);
      }
      else if (sorted && (i > 1) && (xd[i-1] < xd[i])) {
         sorted = false;
      }
   }
   

   
   if (sorted)
      return x; // it's sorted - return as-is
   
   std::vector<double> myvector(xd, xd+n);
   std::sort(myvector.begin(), myvector.end(), comparer_greater);
   
   SEXP ret;
   PROTECT(ret = allocVector(REALSXP, n));
   R_len_t i = 0;
   for (std::vector<double>::iterator it=myvector.begin(); it!=myvector.end(); ++it)
      REAL(ret)[i++] = *it;
   UNPROTECT(1);
   return ret;
}



/**
 * Prepare sorted numeric vector with elements in [0,\infty]
 * 
 * if x is not numeric (or not coercible to), throw error.
 * if of length 0, return numeric(0).
 * if any NA, return NA_real_.
 * if any not in [0,\infty], throw error.
 * if not sorted, return sorted.
 * otherwise, return as-is
 * 
 * 
 * @param numeric vector
 * @param argname argument name (message formatting)
 * @return numeric vector, sorted non-increasingly
 */
SEXP prepare_arg_numeric_sorted_0_infty(SEXP x, const char* argname)
{
   x = prepare_arg_double(x, argname);
   R_len_t n = LENGTH(x);
   if (n <= 0)
      return x; // empty vector => return an empty vector
      
   double* xd = REAL(x);
   bool sorted = true;
   for (R_len_t i=0; i<n; ++i) {
      if (R_IsNA(xd[i])) {
         return ScalarReal(NA_REAL);
      }
      else if (xd[i] < 0) {
         error(MSG__ARG_NOT_IN_O_INFTY, argname);
      }
      else if (sorted && (i > 1) && (xd[i-1] < xd[i])) {
         sorted = false;
      }
   }
   

   
   if (sorted)
      return x; // it's sorted - return as-is
   
   std::vector<double> myvector(xd, xd+n);
   std::sort(myvector.begin(), myvector.end(), comparer_greater);
   
   SEXP ret;
   PROTECT(ret = allocVector(REALSXP, n));
   R_len_t i = 0;
   for (std::vector<double>::iterator it=myvector.begin(); it!=myvector.end(); ++it)
      REAL(ret)[i++] = *it;
   UNPROTECT(1);
   return ret;
}



/* The functions below are borrowed from our `stringi` package for R
   Thanks to Bartek Tartanus :)
 */


/**
 * Prepare character vector argument
 * 
 * If the object cannot be coerced, then an error will be generated
 * 
 * @param x a character vector or an object that can be coerced to a character vector
 * @param argname argument name (message formatting)
 * @return character vector
 * 
 * @version 0.1 (Marek Gagolewski)
 * @version 0.2 (Marek Gagolewski) - argname added
 */
SEXP prepare_arg_string(SEXP x, const char* argname)
{
   if (isString(x))
      return x; // return as-is
   else if (isFactor(x))
   {
      SEXP call;
      PROTECT(call = lang2(install("as.character"), x));
		x = eval(call, R_GlobalEnv); // this will mark it's encoding manually
		UNPROTECT(1);
      return x;
   }
   else if (isVectorAtomic(x))
      return coerceVector(x, STRSXP);
   else if (isSymbol(x))
      return ScalarString(PRINTNAME(x));
      
   error(MSG__ARG_EXPECTED_STRING, argname);
   return x; // avoid compiler warning
}



/**
 * Prepare numeric vector argument
 * 
 * If the object cannot be coerced, then an error will be generated
 * 
 * @param x a numeric vector or an object that can be coerced to a numeric vector
 * @param argname argument name (message formatting)
 * @return numeric vector
 * 
 * @version 0.1 (Bartek Tartanus)
 * @version 0.2 (Marek Gagolewski) - argname added
 */
SEXP prepare_arg_double(SEXP x, const char* argname)
{
   if (isFactor(x)) 
   {
      SEXP call;
      PROTECT(call = lang2(install("as.character"), x));
      x = eval(call, R_GlobalEnv); // this will mark it's encoding manually
		UNPROTECT(1);
      return coerceVector(x, REALSXP);
   }
   else if(isReal(x))
      return x; //return as-is
   else if (isVectorAtomic(x))
      return coerceVector(x, REALSXP);
      
   error(MSG__ARG_EXPECTED_NUMERIC, argname);
   return x; // avoid compiler warning
}


/**
 * Prepare integer vector argument
 * 
 * If the object cannot be coerced, then an error will be generated
 * 
 * @param x an integer vector or an object that can be coerced to an integer vector
 * @param argname argument name (message formatting)
 * @return integer vector
 * 
 * @version 0.1 (Bartek Tartanus)
 * @version 0.2 (Marek Gagolewski) - argname added
 */
SEXP prepare_arg_integer(SEXP x, const char* argname)
{
   if (isFactor(x)) // factors must be checked first (as they are currently represented as integer vectors)
   {
      SEXP call;
      PROTECT(call = lang2(install("as.character"), x));
   	x = eval(call, R_GlobalEnv); // this will mark it's encoding manually
		UNPROTECT(1);
      return coerceVector(x, INTSXP);
   }
   else if (isInteger(x))
      return x; // return as-is
   else if (isVectorAtomic(x))
      return coerceVector(x, INTSXP);
      
   error(MSG__ARG_EXPECTED_INTEGER, argname);
   return x; // avoid compiler warning
}


/**
 * Prepare logical vector argument
 * 
 * If the object cannot be coerced, then an error will be generated
 * 
 * @param x a logical vector or an object that can be coerced to a logical vector
 * @param argname argument name (message formatting)
 * @return logical vector
 * 
 * @version 0.1 (Bartek Tartanus)
 * @version 0.2 (Marek Gagolewski) - argname added
 */
SEXP prepare_arg_logical(SEXP x, const char* argname)
{
   if (isFactor(x))
   {
      SEXP call;
      PROTECT(call = lang2(install("as.character"), x));
      x = eval(call, R_GlobalEnv); // this will mark it's encoding manually
		UNPROTECT(1);
      return coerceVector(x, LGLSXP);
   }
   else if (isLogical(x))
      return x; // return as-is
   else if (isVectorAtomic(x))
      return coerceVector(x, LGLSXP);
      
   error(MSG__ARG_EXPECTED_LOGICAL, argname);
   return x; // avoid compiler warning
}





/** Prepare string argument - one string
 * 
 * If there are 0 elements -> error
 * If there are >1 elements -> warning
 * 
 * @param x R object to be checked/coerced
 * @param argname argument name (message formatting)
 * @return always an R character vector with >=1 element
 * 
 * @version 0.1 (Marek Gagolewski)
 */
SEXP prepare_arg_string_1(SEXP x, const char* argname)
{
   x = prepare_arg_string(x, argname);
   R_len_t nx = LENGTH(x);
   
   if (nx <= 0)
      error(MSG__ARG_EXPECTED_NOT_EMPTY, argname);
   
   if (nx > 1) {
      warning(MSG__ARG_EXPECTED_1_STRING, argname);
//      SEXP xold = x;
//      PROTECT(x = allocVector(STRSXP, 1));
//      SET_STRING_ELT(x, 0, STRING_ELT(xold, 0));
//      UNPROTECT(1);      
   }
   
   return x;
}


/** Prepare double argument - one value
 * 
 * If there are 0 elements -> error
 * If there are >1 elements -> warning
 * 
 * @param x R object to be checked/coerced
 * @param argname argument name (message formatting)
 * @return always an R double vector with >=1 element
 * 
 * @version 0.1 (Marek Gagolewski)
 */
SEXP prepare_arg_double_1(SEXP x, const char* argname)
{
   x = prepare_arg_double(x, argname);
   R_len_t nx = LENGTH(x);
   
   if (nx <= 0)
      error(MSG__ARG_EXPECTED_NOT_EMPTY, argname);
   
   if (nx > 1) {
      warning(MSG__ARG_EXPECTED_1_NUMERIC, argname);
//      double x0 = REAL(x)[0];
//      PROTECT(x = allocVector(REALSXP, 1));
//      REAL(x)[0] = x0;
//      UNPROTECT(1);      
   }
   
   return x;
}


/** Prepare integer argument - one value
 * 
 * If there are 0 elements -> error
 * If there are >1 elements -> warning
 * 
 * @param x R object to be checked/coerced
 * @param argname argument name (message formatting)
 * @return always an R integer vector with >=1 element
 * 
 * @version 0.1 (Marek Gagolewski)
 */
SEXP prepare_arg_integer_1(SEXP x, const char* argname)
{
   x = prepare_arg_integer(x, argname);
   R_len_t nx = LENGTH(x);
   
   if (nx <= 0)
      error(MSG__ARG_EXPECTED_NOT_EMPTY, argname);
   
   if (nx > 1) {
      warning(MSG__ARG_EXPECTED_1_INTEGER, argname);
//      int x0 = INTEGER(x)[0];
//      PROTECT(x = allocVector(INTSXP, 1));
//      INTEGER(x)[0] = x0;
//      UNPROTECT(1);      
   }
   
   return x;   
}


/** Prepare logical argument - one value
 * 
 * If there are 0 elements -> error
 * If there are >1 elements -> warning
 * 
 * @param x R object to be checked/coerced
 * @param argname argument name (message formatting)
 * @return always an R logical vector with >=1 element
 * 
 * @version 0.1 (Marek Gagolewski)
 */
SEXP prepare_arg_logical_1(SEXP x, const char* argname)
{
   x = prepare_arg_logical(x, argname);
   R_len_t nx = LENGTH(x);
   
   if (nx <= 0)
      error(MSG__ARG_EXPECTED_NOT_EMPTY, argname);
   
   if (nx > 1) {
      warning(MSG__ARG_EXPECTED_1_LOGICAL, argname);
//      int x0 = LOGICAL(x)[0];
//      PROTECT(x = allocVector(LGLSXP, 1));
//      LOGICAL(x)[0] = x0;
//      UNPROTECT(1);      
   }
   
   return x;      
}


/** 
 *  Creates a numeric vector filled with \code{NA_real_}
 * 
 *  @param howmany length of the vector, \code{howmany >= 0}
 *  @return a numeric vector of length \code{howmany}
 * 
 * @version 0.1 (Marek Gagolewski)
*/
SEXP vector_NA_double(R_len_t howmany)
{
   if (howmany < 0)
      error(MSG__INCORRECT_INTERNAL_ARG);
   
   SEXP ret;
   PROTECT(ret = allocVector(REALSXP, howmany));
   for (R_len_t i=0; i<howmany; ++i)
      REAL(ret)[i] = NA_REAL;
   UNPROTECT(1);
   
   return ret;   
}

