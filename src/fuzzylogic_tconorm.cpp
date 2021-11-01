/* ************************************************************************* *
 * This file is part of the 'agop' library.                                  *
 *                                                                           *
 * Copyleft (c) 2013-2021, Marek Gagolewski <https://www.gagolewski.com>     *
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


/** The minimum t-conorm
 *
 * @param x numeric
 * @param y numeric
 * @return numeric
 */
SEXP tconorm_minimum(SEXP x, SEXP y)
{
   macro_apply_binaryop_x_y(std::max(x_tab[i], y_tab[i]))
}


/** The product t-conorm
 *
 * @param x numeric
 * @param y numeric
 * @return numeric
 */
SEXP tconorm_product(SEXP x, SEXP y)
{
   macro_apply_binaryop_x_y(x_tab[i] + y_tab[i] - x_tab[i] * y_tab[i])
}


/** The Lukasiewicz t-conorm
 *
 * @param x numeric
 * @param y numeric
 * @return numeric
 */
SEXP tconorm_lukasiewicz(SEXP x, SEXP y)
{
   macro_apply_binaryop_x_y(std::min(x_tab[i]+y_tab[i], 1.0))
}


/** The drastic t-conorm
 *
 * @param x numeric
 * @param y numeric
 * @return numeric
 */
SEXP tconorm_drastic(SEXP x, SEXP y)
{
   macro_apply_binaryop_x_y(
      (x_tab[i] > 0.0 && y_tab[i] > 0.0) ? 1.0 : std::max(x_tab[i], y_tab[i])
   )
}


/** The Fodor t-conorm
 *
 * @param x numeric
 * @param y numeric
 * @return numeric
 */
SEXP tconorm_fodor(SEXP x, SEXP y)
{
   macro_apply_binaryop_x_y(
      (x_tab[i] + y_tab[i] >= 1.0) ? 1.0 : std::max(x_tab[i], y_tab[i])
   )
}
