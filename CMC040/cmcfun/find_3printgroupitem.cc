//! \file
//! \brief Search for a Group/Item
// %SBTTL "FIND_3PRINTGROUPITEM"
// %IDENT "V3.6a Calico"
//
// Source: ../../CMC030/cmcfun/source/find_3printgroupitem.bas
// Translated from Basic to C++ using btran
// on Tuesday, January 02, 2018 at 14:28:50
//

#include <cstdlib>
#include <string>
#include <unistd.h>
#include "basicfun.h"

#include "preferences.h"
#include "cmcfun.h"
#include "scopedef.h"
#include "database.h"
#include "report.h"


//! \brief Search for a Group/Item
//!
//!	This function searches for a group/item and returns the
//!	position in the PRINT.ITEM$() array where the actual
//!	information is stored.
//!	.lm -5
//!
//! Parameters:
//!
//!	GROUP$
//!		This passed string is the group the user is searching for.
//!
//!	ITEM$
//!		This passed string is the item the user is looking for.
//!
//!
//!	This function returns the position of the group/item in
//!	the PRINT.ITEM$() array
//!
//! Example:
//!
//!	GROUP% = FIND_3PRINTGROUPITEM("RQ","*")
//!
//! \aauthor 10/20/92 - Kevin Handy
//!
long find_3printgroupitem(
	const std::string &group,
	const std::string &item,
	printx_cdd &printx)
{
	long Result;
	long i;
	long j;

	//
	// Search for group
	//
	for (i = 1; i <= printx.groups; i++)
	{
		if (group == printx.groupx[i])
		{
			goto L_1000;
		}
	}
	Result = 0;
	return Result;
	//
L_1000:;
	// Search for item within group
	//
	for (j = (printx.groupp[i] & 2047);
		j <= (printx.groupp[i + 1] & 2047) - 1; j++)
	{
		if (comp_string(boost::trim_right_copy(item),
			boost::trim_right_copy(printx.item[j])))
		{
			goto L_2000;
		}
	}
	Result = 0;
	return Result;
	//
L_2000:;
	// We have found the group/item
	//
	Result = j;
	return Result;
}
