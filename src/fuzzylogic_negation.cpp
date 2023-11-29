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


/** The classic fuzzy negation
 *
 * @param x numeric
 * @return numeric
 */
SEXP fnegation_classic(SEXP x)
{
   macro_apply_unaryop_x(1.0-x_tab[i])
}


/** The Yager fuzzy negation
 *
 * @param x numeric
 * @return numeric
 */
SEXP fnegation_yager(SEXP x)
{
   macro_apply_unaryop_x(sqrt(1.0-x_tab[i]*x_tab[i]))
}


/** The minimal fuzzy negation
 *
 * @param x numeric
 * @return numeric
 */
SEXP fnegation_minimal(SEXP x)
{
   macro_apply_unaryop_x((x_tab[i]==0.0)?1.0:0.0)
}


/** The maximal fuzzy negation
 *
 * @param x numeric
 * @return numeric
 */
SEXP fnegation_maximal(SEXP x)
{
   macro_apply_unaryop_x((x_tab[i]<1.0)?1.0:0.0)
}
