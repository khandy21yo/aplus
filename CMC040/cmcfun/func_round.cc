//! \file
//! \brief Function to Round Numbers
//! %SBTTL "FUNC_ROUND"
//! %IDENT "V3.6 Calico"
//!

/*
 * Include files
 */
#include "cmcfun.h"
#include <math.h>

//!
//! \brief sgn - Sign function.
//!
//!	Returnd the sign of the number as (1, 0, -1)
//!/
static inline double sgn(double x)
{
	return (((x) > 0.0) ? 1.0 : (((x) < 0.0) ? -1.0 : 0.0));
}

//! \brief Function to Round Numbers
//!
//!	This function rounds off the numbers the user asks for.
//!
//! \returns This function returns the number the user entered as it
//!	looks rounded off.
//!
//! Example:
//!
//!	ROUND = FUNC_ROUND( 3.14159, 3%)
//!
double func_round(
	double xnum,
		//!< The passed number the user wants to have rounded off.
	int xprec)
		//!< The passed precision off the rounded off number.
{
	/*
	 * So we don't have to calculate it several times
	 */
	double Power = pow(10.0, xprec);
	return (floor(fabs(xnum) * Power + 0.5001) / Power) * sgn(xnum);
}
