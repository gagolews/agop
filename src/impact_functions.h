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




#ifndef __impact_functions_h
#define __impact_functions_h

#include "agop.h"



void index_h(double* x, int* n, double* out);


int __index_h_log(double* x, int n);

void index_h_log(double* x, int* n, double* out);

void index_g(double* x, int* n, double* out);

void Sstat2(double* x, int* n, double* out);

void index_rp_finite(double* x, int* n, double *p, double* out);

void index_rp_infinite(double* x, int* n, double* out);



int __index_lp_finite_testContains(double uk, double vk, double p, double ui, double vi, double uj, double vj);

void __index_lp_finite_getAB(double p, double ui, double vi, double uj, double vj, double* a2p, double* b2p);

void index_lp_finite(double* x, int* n, double *p, int* s, double* out);

void index_lp_infinite(double* x, int* n, double* out);


#endif
