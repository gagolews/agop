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


/** The minimum t-norm
 *
 * @param x numeric
 * @param y numeric
 * @return numeric
 */
SEXP tnorm_minimum(SEXP x, SEXP y)
{
   macro_apply_binaryop_x_y(std::min(x_tab[i], y_tab[i]))
}


/** The product t-norm
 *
 * @param x numeric
 * @param y numeric
 * @return numeric
 */
SEXP tnorm_product(SEXP x, SEXP y)
{
   macro_apply_binaryop_x_y(x_tab[i] * y_tab[i])
}


/** The Lukasiewicz t-norm
 *
 * @param x numeric
 * @param y numeric
 * @return numeric
 */
SEXP tnorm_lukasiewicz(SEXP x, SEXP y)
{
   macro_apply_binaryop_x_y(std::max(x_tab[i]+y_tab[i]-1.0, 0.0))
}


/** The drastic t-norm
 *
 * @param x numeric
 * @param y numeric
 * @return numeric
 */
SEXP tnorm_drastic(SEXP x, SEXP y)
{
   macro_apply_binaryop_x_y(
      (x_tab[i] < 1.0 && y_tab[i] < 1.0) ? 0.0 : std::min(x_tab[i], y_tab[i])
   )
}


/** The Fodor t-norm
 *
 * @param x numeric
 * @param y numeric
 * @return numeric
 */
SEXP tnorm_fodor(SEXP x, SEXP y)
{
   macro_apply_binaryop_x_y(
      (x_tab[i] + y_tab[i] <= 1.0) ? 0.0 : std::min(x_tab[i], y_tab[i])
   )
}
