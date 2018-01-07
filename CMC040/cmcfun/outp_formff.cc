//! \file
//! \brief  Form Feed to a Report
// %SBTTL "OUTP_FORMFF"
// %IDENT "V3.6a Calico"
////
// Source: ../../CMC030/cmcfun/source/outp_formff.bas
// Translated from Basic to C++ using btran
// on Saturday, January 06, 2018 at 20:21:35
//

#include <iostream>
#include <cstdlib>
#include <cstring>
#include <unistd.h>
#include "basicfun.h"

#include "preferences.h"
#include "cmcfun.h"
#include "scopedef.h"
#include "smg/smg.h"
#include "database.h"
#include "cmcfun/report.h"
#include "utl/utl_reportx.h"



//!
//!
//! Abstract:HELP
//!	.p
//! Parameters:
//!
//!	UTL_REPORTX
//!		The report that the form feed is sent to.
//!
//!	It returns a form feed to the report of the programmer's choice.
//!
//! Example:
//!
//!	CALL OUTP_FORMFF(UTL_REPORTX)
//!
//! AUTHOR:
//!
//!	04/27/87 - Kevin Handy
//!
void outp_formff(
	utl_reportx_cdd &utl_reportx)
{
	long i;
	std::string nextpage;

	//
	// If form feed isn't defined, fake up something.
	//
	if (utl_reportx.nextpage == "")
	{
		//
		// Must print to a local printer
		//
		for (i = utl_reportx.lineno; i <= utl_reportx.pagelen - 1; i++)
		{
			utl_reportx.chan << std::endl;
		}
	}
	else
	{
		//
		// Form feed everywhere else
		//
		writ_string(utl_reportx.nextpage, nextpage);
		utl_reportx.chan << nextpage;
	}
	utl_reportx.lineno = 0;
	utl_reportx.pageno = utl_reportx.pageno + 1;
}
