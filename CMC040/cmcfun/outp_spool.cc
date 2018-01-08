//! \file
//! \brief OUTP_SPOOL -- Print Spooler Routing Function
//
#pragma module outp_spool "V3.6 Calico"

#include <iostream>
#include <string>
#include <vector>

#include "preferences.h"
#include "scopedef.h"
#include "smg/smg.h"
#include "cmcfun/report.h"
#include "utl/utl_reportx.h"


//!
//! ABSTRACT:
//!
//!	This function is used to send a report to the
//!	spooler when the processing is finished.
//!
//!	This function will try to mount a form if it is
//!	given a form name.
//!
//!	NOTE: No errors are returned back.
//!
//! Parameters:
//!
//!	UTL_REPORTX
//!		Passed report structure containing all necessary
//!		information to spool the report.
//!
//!
//!	Returned value
//!		This function sends a report on to the
//!		spooler when the print processing has finished.
//!
//! AUTHOR:
//!
//!	11/24/86 - B. Craig Larsen
//!		VAX Version
//!
//!	05/17/88 - Kevin Handy
//!		Conversion to a function.
//!
void outp_spool(
	utl_reportx_cdd &utl_reportx)
{
	//! \todo This doesn'y handle things properly
	std::string cmd = "lpr " + utl_reportx.defout;
	system(cmd.c_str());
}
