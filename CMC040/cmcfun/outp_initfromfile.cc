//! \file
//! \file
//! \brief Initilize REPORT Output Information
//! %SBTTL "OUTP_INITFROMFILE"
// %IDENT "V3.6a Calico"
//
// Source: ../../CMC030/cmcfun/source/outp_initfromfile.bas
// Translated from Basic to C++ using btran
// on Tuesday, December 26, 2017 at 17:18:38
//

#include <cstdlib>
#include <cstring>
#include <unistd.h>
#include "basicfun.h"

#include "preferences.h"
$include "cmcfun,h""
#include "cmcfun/report.h"
#include "utl/utl_reportx.h"

extern scope_struct scope;

//
//
// Abstract:HELP
//	.p
//	This function initilizes the REPORT output functions.
//
// Parameters:
//
//	UTL_REPORTX
//		The file used to initilize the report functions.
//
//	XWIDTH
//		The returned variable used for the report output.
//
//	Returned value
//		Initilizes the report output functions and other
//		information the file has.
//
// Author:
//
//	10/19/95 - Kevin Handy
//		Original function rename to OUTP_36INITFROMFILE,
//		with different parameters. This function written
//		to reference the changed function.
//

void outp_initfromfile(
	utl_reportx_cdd utl_reportx,
	int xwidth)
{
	//
	// Open keyboard if not open
	//
	if (scope.smg_option == 0)
	{
		read_initialize();
	}
	outp_36initfromfile(scope, utl_reportx, xwidth);
}
