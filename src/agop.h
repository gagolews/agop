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




#ifndef __agop_h
#define __agop_h

#ifndef NDEBUG
#define NDEBUG
#endif

#include <iostream>
#include <algorithm>
#include <vector>
#include <deque>
#include <cfloat>
using namespace std;

#define R_NO_REMAP

#include <R.h>
#include <Rmath.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>




// length macro conflicts with STL
#undef length

#define EPS sqrt(DBL_EPSILON)

#define MSG__WARN_RECYCLE \
   "longer object length is not a multiple of shorter object length"

#define MSG__ARGS_EXPECTED_EQUAL_SIZE \
   "`%s` and `%s` should be of equal lengths"

#define MSG_ARG_TOO_SHORT \
   "not enough elements in `%s`"

#define MSG__ARG_NOT_GE_A \
   "all elements in `%s` should be not less than %g"

#define MSG__ARG_NOT_GT_A \
   "all elements in `%s` should be greater than %g"

#define MSG__ARG_NOT_LE_B \
   "all elements in `%s` should be not greater than %g"

#define MSG__ARG_NOT_IN_AB \
   "all elements in `%s` should be in [%g,%g]"

#define MSG__ARG_EXPECTED_NOT_NA \
   "missing value in argument `%s` is not supported"

#define MSG__ARG_EXPECTED_NOT_EMPTY \
   "argument `%s` should be a non-empty vector"

#define MSG__ARG_EXPECTED_1_STRING \
   "argument `%s` should be one character string; taking the first one"

#define MSG__ARG_EXPECTED_1_LOGICAL \
   "argument `%s` should be one logical value; taking the first one"

#define MSG__ARG_EXPECTED_1_INTEGER \
   "argument `%s` should be one integer value; taking the first one"

#define MSG__ARG_EXPECTED_1_NUMERIC \
   "argument `%s` should be one numeric value; taking the first one"

#define MSG__ARG_EXPECTED_STRING \
   "argument `%s` should be a character vector (or an object coercible to)"

#define MSG__ARG_EXPECTED_LOGICAL \
   "argument `%s` should be a logical vector (or an object coercible to)"

#define MSG__ARG_EXPECTED_INTEGER \
   "argument `%s` should be an integer vector (or an object coercible to)"

#define MSG__ARG_EXPECTED_NUMERIC \
   "argument `%s` should be a numeric vector (or an object coercible to)"

#define MSG__INCORRECT_INTERNAL_ARG \
   "incorrect argument"

#define MSG__INTERNAL_ERROR \
   "internal error"

#define MSG__DIM_LENGTH \
   "incorrect number of dimensions in %s"

#define MSG__DIM_NOTEQUAL \
   "dimensions in %s should be equal"

#define MSG__EXPECTED_ACYCLIC \
   "%s should be acyclic"

struct double2 {
   double v1;
   double v2;
   double2(double _v1=0.0, double _v2=0.0) {this->v1=_v1; this->v2=_v2;}
   SEXP toR() {
      SEXP ret;
      PROTECT(ret = Rf_allocVector(REALSXP, 2));
      REAL(ret)[0] = this->v1;
      REAL(ret)[1] = this->v2;
      UNPROTECT(1);
      return ret;
   }
};

SEXP vector_NA_double(R_len_t howmany); // internal
void check_range(double* x, double n, double xmin, double xmax, const char* argname);

SEXP prepare_arg_numeric(SEXP x, const char* argname);
SEXP prepare_arg_numeric_sorted_dec(SEXP x, const char* argname);
SEXP prepare_arg_numeric_sorted_inc(SEXP x, const char* argname);

SEXP prepare_arg_string(SEXP x, const char* argname);
SEXP prepare_arg_double(SEXP x, const char* argname);
SEXP prepare_arg_integer(SEXP x, const char* argname);
SEXP prepare_arg_logical(SEXP x, const char* argname);
SEXP prepare_arg_string_1(SEXP x, const char* argname);
SEXP prepare_arg_double_1(SEXP x, const char* argname);
SEXP prepare_arg_integer_1(SEXP x, const char* argname);
SEXP prepare_arg_logical_1(SEXP x, const char* argname);
SEXP prepare_arg_logical_square_matrix(SEXP x, const char* argname);

SEXP index_h(SEXP x);
SEXP index_g(SEXP x);
SEXP index_g_zi(SEXP x);
SEXP index_maxprod(SEXP x);
SEXP index_w(SEXP x);
SEXP index_rp(SEXP x, SEXP p);
SEXP index_lp(SEXP x, SEXP p);

SEXP owa(SEXP x, SEXP w);
SEXP wam(SEXP x, SEXP w);
SEXP owmax(SEXP x, SEXP w);
SEXP wmax(SEXP x, SEXP w);
SEXP owmin(SEXP x, SEXP w);
SEXP wmin(SEXP x, SEXP w);

SEXP d2owa_checkwts(SEXP w);

SEXP check_comonotonicity(SEXP x, SEXP y, SEXP incompatible_lengths);
SEXP pord_weakdom(SEXP x, SEXP y);
SEXP pord_nd(SEXP x, SEXP y, SEXP incompatible_lengths);
SEXP pord_spread(SEXP x, SEXP y, SEXP incompatible_lengths);

SEXP rel_is_cyclic(SEXP x);

SEXP rel_is_irreflexive(SEXP x);

SEXP rel_is_asymmetric(SEXP x);

SEXP rel_is_antisymmetric(SEXP x);

SEXP rel_is_reflexive(SEXP x);
SEXP rel_closure_reflexive(SEXP x);
SEXP rel_reduction_reflexive(SEXP x);

SEXP rel_is_symmetric(SEXP x);
SEXP rel_closure_symmetric(SEXP x);

SEXP rel_is_total(SEXP x);
SEXP rel_closure_total_fair(SEXP x);

SEXP rel_is_transitive(SEXP x);
SEXP rel_closure_transitive(SEXP x);
SEXP rel_reduction_transitive(SEXP x);

SEXP rel_reduction_hasse(SEXP x);

SEXP exp_test_statistic(SEXP x);
SEXP ppareto2(SEXP q, SEXP k, SEXP s, SEXP lower_tail);


#define macro_apply_binaryop_x_y(op) \
   x = prepare_arg_double(x, "x"); \
   y = prepare_arg_double(y, "y"); \
   R_len_t x_length = LENGTH(x);   \
   R_len_t y_length = LENGTH(y);   \
   double* x_tab = REAL(x);        \
   double* y_tab = REAL(y);        \
   if (x_length <= 0) Rf_error(MSG_ARG_TOO_SHORT, "x");  \
   if (y_length <= 0) Rf_error(MSG_ARG_TOO_SHORT, "y");  \
   if (x_length != y_length)                             \
      Rf_error(MSG__ARGS_EXPECTED_EQUAL_SIZE, "x", "y"); \
   SEXP res;                                             \
   PROTECT(res = Rf_allocVector(REALSXP, x_length));     \
   double* res_tab = REAL(res);                          \
   for (R_len_t i=0; i<x_length; ++i) {                  \
      if (ISNA(x_tab[i]) || ISNA(y_tab[i]))              \
         res_tab[i] = NA_REAL;                           \
      else if (x_tab[i] < 0.0 || x_tab[i] > 1.0)         \
         Rf_error(MSG__ARG_NOT_IN_AB, "x", 0.0, 1.0);    \
      else if (y_tab[i] < 0.0 || y_tab[i] > 1.0)         \
         Rf_error(MSG__ARG_NOT_IN_AB, "y", 0.0, 1.0);    \
      else                                               \
         res_tab[i] = (op);                              \
   }                                                     \
   UNPROTECT(1);                                         \
   return res;

#define macro_apply_unaryop_x(op)  \
   x = prepare_arg_double(x, "x"); \
   R_len_t x_length = LENGTH(x);   \
   double* x_tab = REAL(x);        \
   if (x_length <= 0) Rf_error(MSG_ARG_TOO_SHORT, "x");  \
   SEXP res;                                             \
   PROTECT(res = Rf_allocVector(REALSXP, x_length));     \
   double* res_tab = REAL(res);                          \
   for (R_len_t i=0; i<x_length; ++i) {                  \
      if (ISNA(x_tab[i]))                                \
         res_tab[i] = NA_REAL;                           \
      else if (x_tab[i] < 0.0 || x_tab[i] > 1.0)         \
         Rf_error(MSG__ARG_NOT_IN_AB, "x", 0.0, 1.0);    \
      else                                               \
         res_tab[i] = (op);                              \
   }                                                     \
   UNPROTECT(1);                                         \
   return res;

SEXP tnorm_minimum(SEXP x, SEXP y);
SEXP tnorm_lukasiewicz(SEXP x, SEXP y);
SEXP tnorm_fodor(SEXP x, SEXP y);
SEXP tnorm_product(SEXP x, SEXP y);
SEXP tnorm_drastic(SEXP x, SEXP y);

SEXP tconorm_minimum(SEXP x, SEXP y);
SEXP tconorm_lukasiewicz(SEXP x, SEXP y);
SEXP tconorm_fodor(SEXP x, SEXP y);
SEXP tconorm_product(SEXP x, SEXP y);
SEXP tconorm_drastic(SEXP x, SEXP y);

SEXP fnegation_classic(SEXP x);
SEXP fnegation_yager(SEXP x);
SEXP fnegation_minimal(SEXP x);
SEXP fnegation_maximal(SEXP x);

SEXP fimplication_minimal(SEXP x, SEXP y);
SEXP fimplication_maximal(SEXP x, SEXP y);
SEXP fimplication_kleene(SEXP x, SEXP y);
SEXP fimplication_lukasiewicz(SEXP x, SEXP y);
SEXP fimplication_reichenbach(SEXP x, SEXP y);
SEXP fimplication_fodor(SEXP x, SEXP y);
SEXP fimplication_goguen(SEXP x, SEXP y);
SEXP fimplication_goedel(SEXP x, SEXP y);
SEXP fimplication_rescher(SEXP x, SEXP y);
SEXP fimplication_weber(SEXP x, SEXP y);
SEXP fimplication_yager(SEXP x, SEXP y);

#endif
