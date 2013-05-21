/* ************************************************************************* *
 *   This file is part of the `agop` library.                                *
 *                                                                           *
 *   Copyright 2013 Marek Gagolewski, Anna Cena                              *
 *                                                                           *
 *   Parts of the code are taken from the 'CITAN' R package by M. Gagolewski *                                                                       *
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

// #define NDEBUG

#include <R.h>
#include <Rmath.h>
#include <Rdefines.h>
#include <R_ext/Rdynload.h>

#include <iostream>
#include <algorithm>    
#include <vector>       
using namespace std;



#define MSG__ARG_NOT_IN_O_INFTY \
   "all elements in `%s` should be in [0,Inf]"

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
   
   
   
SEXP vector_NA_double(R_len_t howmany); // internal
SEXP prepare_arg_numeric_sorted_0_infty(SEXP x, const char* argname); // @TODO: R-interface, not in NAMESPACE
SEXP prepare_arg_string(SEXP x, const char* argname); // @TODO: R-interface, not in NAMESPACE
SEXP prepare_arg_double(SEXP x, const char* argname); // @TODO: R-interface, not in NAMESPACE
SEXP prepare_arg_integer(SEXP x, const char* argname); // @TODO: R-interface, not in NAMESPACE
SEXP prepare_arg_logical(SEXP x, const char* argname); // @TODO: R-interface, not in NAMESPACE
SEXP prepare_arg_string_1(SEXP x, const char* argname); // @TODO: R-interface, not in NAMESPACE
SEXP prepare_arg_double_1(SEXP x, const char* argname); // @TODO: R-interface, not in NAMESPACE
SEXP prepare_arg_integer_1(SEXP x, const char* argname); // @TODO: R-interface, not in NAMESPACE
SEXP prepare_arg_logical_1(SEXP x, const char* argname); // @TODO: R-interface, not in NAMESPACE


SEXP index_h(SEXP x); // the h-index
int __index_h_log(double* x, int n); // internal, currently not used




void index_g(double* x, int* n, double* out); // @TODO: rewrite

void Sstat2(double* x, int* n, double* out); // @TODO: rewrite

void index_rp_finite(double* x, int* n, double *p, double* out); // @TODO: rewrite

void index_rp_infinite(double* x, int* n, double* out); // @TODO: rewrite



int __index_lp_finite_testContains(double uk, double vk, double p, double ui, double vi, double uj, double vj); // @TODO: rewrite

void __index_lp_finite_getAB(double p, double ui, double vi, double uj, double vj, double* a2p, double* b2p); // @TODO: rewrite

void index_lp_finite(double* x, int* n, double *p, int* s, double* out); // @TODO: rewrite

void index_lp_infinite(double* x, int* n, double* out); // @TODO: rewrite





void pareto2_phirsch(double* x, int* m, double* n, double* k, double* s); // @TODO: rewrite

void pareto2_dhirsch(double* x, int* m, double* n, double* k, double* s); // @TODO: rewrite








#endif
