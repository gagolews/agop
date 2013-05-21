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




#include "agop.h"


void pareto2_phirsch(double* x, int* m, double* n, double* k, double* s)
{
	int M = *m;
	double N = *n;
	double K = *k;
	double S = *s;
	for (int i=0; i<M; ++i)
	{
		if      (x[i]>N-1e-9)     x[i] = 1.0;
		else if (x[i]<0.0)        x[i] = 0.0;
		else
		{
			double floorx = floor(x[i]);
			x[i] = pbeta(1.0-pow(S/(S+floorx+1.0),K),
				N-floorx,
				floorx+1.0,
				1, 0);
		}
	}
}



void pareto2_dhirsch(double* x, int* m, double* n, double* k, double* s)
{
	int M = *m;
	double N = *n;
	double K = *k;
	double S = *s;
	for (int i=0; i<M; ++i)
	{
		if (x[i] > 1e-9 && x[i] < N-1e-9)
		{
			double floorx = floor(x[i]+1e-9);
			x[i] = pbeta(1.0-pow(S/(S+floorx+1.0),K),
				N-floorx,
				floorx+1.0,
				1, 0)
			      -pbeta(1.0-pow(S/(S+floorx),K),
				N-floorx+1.0,
				floorx,
				1, 0);
		}
		else if (x[i] > N+1e-9 || x[i] < 0.0) x[i] = 0.0;
		else if (x[i] > N-1e-9)
		{
			double floorx = floor(x[i]-1e-9);
			x[i] = 1.0-pbeta(1.0-pow(S/(S+floorx+1.0),K),
				N-floorx,
				floorx+1.0,
				1, 0);
		}
		else /* if (x[i] < 1e-9) */
		{
			double floorx = floor(x[i]+1e-9);
			x[i] = pbeta(1.0-pow(S/(S+floorx+1.0),K),
				N-floorx,
				floorx+1.0,
				1, 0);
		}
	}
}



