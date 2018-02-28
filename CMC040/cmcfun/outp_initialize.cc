//! \file
//! \brief Initilize for Printer Control Sequences
// %SBTTL "OUTP_INITIALIZE"
// %IDENT "V3.6a Calico"
//
// Source: ../../CMC030/cmcfun/source/outp_initialize.bas
// Translated from Basic to C++ using btran
// on Tuesday, January 16, 2018 at 22:47:53
//

#include <string>
#include <cstdlib>
#include <cstring>
#include <unistd.h>
#include "basicfun.h"

#include "preferences.h"
#include "cmcfun.h"
#include "scopedef.h"
#include "cmcfun/report.h"

#include "smg/lbr.h"


extern scope_struct scope;
printx_cdd printx;



// ++
//
// Abstract:HELP
//	.p
//	This function reads in a printer definition file into
//	the working arrays.
//
// Parameters:
//
//	PRINT_TYPE$
//		The passed printer definition file the user enters.
//
//	Returned value
//		Puts a printer definition file into the working
//		arrays.
//
// Example:
//
//	CALL OUTP_INITIALIZE("PRINT.COM")
//
// AUTHOR:
//
//	07/22/86 - Kevin Handy
//
void outp_initialize(
	const std::string &print_type)
{
	long i;
	std::string inline_V1;
	long j;
	lbr_index_cdd lr_index;
	long st;
	std::string text;
	long txrfa;

	//
	// Declare variables
	//
	printx.items = 0;
	printx.groups = 0;
	if (print_type == "")
	{
		return;
	}
	//
	// Set up the control structure if necessary
	//
	if ((lbr$ini_control(lr_index, LBR$C_READ) & 1) == 0)
	{
		entr_3message(scope, "Unable to initilize library", 0);
		return;
	}
	//
	// Open work file
	//
	if ((lbr$open(lr_index, "CMC:PRINT_TYPE", 0, "CMC:.TLB") & 1) == 0)
	{
		entr_3message(scope, "Unable to open library CMC:PRINT_TYPE", 0);
		return;
	}
	//
	// Search for key in file
	//
	if ((lbr$lookup_key(lr_index, print_type, txrfa) & 1) == 0)
	{
		entr_3message(scope, std::string("Unable to find PrintType ") + print_type, 0);
		goto closelibrary;
	}
	//
L_2000:;
	// Search for a line
	//
	text = std::string(132, ' ');
	if ((lbr$get_record(lr_index, text) & 1) != 1)
	{
		goto closelibrary;
	}
	//
	// Process line
	//
	inline_V1 = boost::trim_right_copy(text);
	if ((inline_V1 == "") || (inline_V1.substr(0, 1) == "!"))
	{
		goto L_2000;
	}
	//
	// Data item (true) or new group (false)
	//
	if (boost::erase_all_copy(inline_V1.substr(0, 1), " ") == "")
	{
		//
		// Split up item definition
		//
		i = (inline_V1.find(9, 1) + 1);
		if (i == 0)
		{
			i = inline_V1.size() + 1;
		}
		printx.items = printx.items + 1;
		printx.item[printx.items] = basic::Qseg(inline_V1, 2, i - 1);
		printx.sequ[printx.items] = basic::right(inline_V1, i + 1);
	}
	else
	{
		//
		// Split up group definition
		//
		printx.groups = printx.groups + 1;
		i = (inline_V1.find(" ", 0) + 1);
		j = (inline_V1.find(" ", i + 1 - 1) + 1);
		//
		// If it starts with a "'", then it is hidden
		//
		if (inline_V1.substr(0, 1) == "'")
		{
			printx.groupp[printx.groups] = (printx.items + 1) | 2048;
			printx.groupx[printx.groups] = basic::Qseg(inline_V1, 2, i - 1);
		}
		else
		{
			printx.groupp[printx.groups] = (printx.items + 1);
			printx.groupx[printx.groups] = inline_V1.substr(0, i - 1);
		}
		printx.deflt[printx.groups] = basic::Qseg(inline_V1, i + 1, j - 1);
		printx.descr[printx.groups] = basic::right(inline_V1, j + 1);
	}
	goto L_2000;
	//
	// Now we are done
	//
closelibrary:;
	st = lbr$close(lr_index);
	printx.groupp[printx.groups + 1] = printx.items + 1;
}
