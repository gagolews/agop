/* ************************************************************************* *
 *   This file is part of the `agop` library.                                *
 *                                                                           *
 *   Copyright 2013-2014 Marek Gagolewski, Anna Cena                         *
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
