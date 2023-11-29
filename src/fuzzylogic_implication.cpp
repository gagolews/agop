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


/** The minimal fuzzy implication
 *
 * @param x numeric
 * @param y numeric
 * @return numeric
 */
SEXP fimplication_minimal(SEXP x, SEXP y)
{
   macro_apply_binaryop_x_y((double)(x_tab[i] == 0.0 || y_tab[i] == 1.0))
}


/** The maximal fuzzy implication
 *
 * @param x numeric
 * @param y numeric
 * @return numeric
 */
SEXP fimplication_maximal(SEXP x, SEXP y)
{
   macro_apply_binaryop_x_y((double)(x_tab[i] < 1.0 || y_tab[i] > 0.0))
}


/** The Kleene-Dienes fuzzy implication
 *
 * @param x numeric
 * @param y numeric
 * @return numeric
 */
SEXP fimplication_kleene(SEXP x, SEXP y)
{
   macro_apply_binaryop_x_y(std::max(1.0-x_tab[i], y_tab[i]))
}


/** The Lukasiewicz fuzzy implication
 *
 * @param x numeric
 * @param y numeric
 * @return numeric
 */
SEXP fimplication_lukasiewicz(SEXP x, SEXP y)
{
   macro_apply_binaryop_x_y(std::min(1.0-x_tab[i]+y_tab[i], 1.0))
}


/** The Reichenbach fuzzy implication
 *
 * @param x numeric
 * @param y numeric
 * @return numeric
 */
SEXP fimplication_reichenbach(SEXP x, SEXP y)
{
   macro_apply_binaryop_x_y(1.0-x_tab[i]+x_tab[i]*y_tab[i])
}


/** The Fodor fuzzy implication
 *
 * @param x numeric
 * @param y numeric
 * @return numeric
 */
SEXP fimplication_fodor(SEXP x, SEXP y)
{
   macro_apply_binaryop_x_y((x_tab[i] <= y_tab[i])?1.0:std::max(1.0-x_tab[i], y_tab[i]))
}


/** The Goguen fuzzy implication
 *
 * @param x numeric
 * @param y numeric
 * @return numeric
 */
SEXP fimplication_goguen(SEXP x, SEXP y)
{
   macro_apply_binaryop_x_y((x_tab[i] <= y_tab[i])?1.0:(y_tab[i]/x_tab[i]))
}


/** The Goedel fuzzy implication
 *
 * @param x numeric
 * @param y numeric
 * @return numeric
 */
SEXP fimplication_goedel(SEXP x, SEXP y)
{
   macro_apply_binaryop_x_y((x_tab[i] <= y_tab[i])?1.0:y_tab[i])
}


/** The Rescher fuzzy implication
 *
 * @param x numeric
 * @param y numeric
 * @return numeric
 */
SEXP fimplication_rescher(SEXP x, SEXP y)
{
   macro_apply_binaryop_x_y((double)(x_tab[i] <= y_tab[i]))
}


/** The Weber fuzzy implication
 *
 * @param x numeric
 * @param y numeric
 * @return numeric
 */
SEXP fimplication_weber(SEXP x, SEXP y)
{
   macro_apply_binaryop_x_y((x_tab[i] < 1.0)?1.0:y_tab[i])
}


/** The Yager fuzzy implication
 *
 * @param x numeric
 * @param y numeric
 * @return numeric
 */
SEXP fimplication_yager(SEXP x, SEXP y)
{
   macro_apply_binaryop_x_y((x_tab[i] == 0.0 && y_tab[i] == 0.0)?1.0:pow(y_tab[i], x_tab[i]))
}
