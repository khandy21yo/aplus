/*
 *	%TITLE "FUNC_MODULUS -- Modulus Function"
 */
#pragma module func_modulus "V3.6 Calico"

/*
 *
 * COPYRIGHT (C) 1988 BY
 * Computer Management Center, Inc.
 * Idaho Falls, Idaho.
 *
 * This software is furnished under a license and may be used and
 * copied only in accordance with terms of such license and with
 * the inclusion of the above copyright notice.  This software or
 * any other copies thereof may not be provided or otherwise made
 * available to any other person.  No title to and ownership of
 * the software is hereby transferred.
 *
 * The information in this software is subject to change without
 * notice and should not be construed as a committment by
 * Computer Management Center, Inc.
 *
 * CMC assumes no responsibility for the use or reliability of
 * its software on equipment which is not supported by CMC.
 *
 *++
 *
 * Abstract:
 *
 *	This function implements the missing modulus function
 *	of basic+2.
 *
 * Parameters:
 *
 *	value1
 *		The passed integer value the user wants to have divided.
 *
 *	value2
 *		The passed integer value the user wants value1 to be 
 *		divided by.
 *
 *
 *	The return is the remainder of the two values.
 *
 * Example:
 *
 *	MOD% = FUNC_MODULUS(5%,2%)
 *
 * Compile:
 *
 *	$ CC FUNC_SOURCE:FUNC_MODULUS/G_FLOAT
 *	$ LIB FUNC_LIB:CMC_3VECTOR/REP FUNC_MODULUS
 *	$ DELETE FUNC_MODULUS.OBJ;*
 *
 * AUTHOR:
 *
 *	06/22/86 - Kevin Handy
 *
 * MODIFICATION HISTORY:
 *
 *	04/17/95 - Kevin Handy
 *		(V3.6)
 *		Update to V3.6 coding standards.
 *
 *	05/27/99 - Kevin Handy
 *		Modify so will compile in DEC-C without errors
 *		(module)
 *--
 */

/*
 * Include files
 */

long func_modulus( long *value1, long *value2 )
{
	return( *value1 % *value2 );
}
