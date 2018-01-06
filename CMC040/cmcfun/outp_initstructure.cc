//! \file
// \brief Initilize Report Structure from Report
// %SBTTL "OUTP_INITSTRUCTURE"
// %IDENT "V3.6a Calico"

//
// Source: ../../CMC030/cmcfun/source/outp_initstructure.bas
// Translated from Basic to C++ using btran
// on Tuesday, January 02, 2018 at 16:15:24
//

#include <cstdlib>
#include <string>
#include <unistd.h>
#include "basicfun.h"

#include "preferences.h"
#include "cmcfun.h"
#include "database.h"
#include "scopedef.h"
#include "cmcfun/report.h"
#include "utl/utl_report.h"


//
// Abstract:HELP
//	.p
//	Initilizes the UTL_REPORTX structure from the
//	UTL_REPORT record.
//
// Parameters:
//
//	UTL_REPORT
//		The passed file the UTL_REPORTX structure is from.
//
//	UTL_REPORTX
//		The returned file that is being created by the UTL_REPORT
//		file.
//
// Example:
//
//	CALL OUTP_INITSTRUCTURE(UTL_REPORT_CDD UTL_REPORT,
//		UTL_REPORTX_CDD UTL_REPORTX)
//
// AUTHOR:
//
//	05/07/87 - Lance Williams
//
//		Use WHEN ERROR IN
void outp_initstructure(
	const utl_report_cdd &utl_report,
	utl_reportx_cdd &utl_reportx)
{
	long i;
	std::string output;
	std::string printto;
	long temp;

	//
	// Include files
	//
	utl_reportx.repnum = utl_report.repnum;
	utl_reportx.prodev = utl_report.prodev;
	utl_reportx.pronam = utl_report.pronam;
	utl_reportx.spool = utl_report.spool;
	utl_reportx.spoolform = utl_report.spoolform;
	for (i = 0; i <= 9; i++)
	{
		utl_reportx.optdef[i] = utl_report.optdef[i];
	}
	output = utl_report.defout;
	temp = (output.find(" ", 0) + 1);
	if (temp)
	{
		printto = boost::trim_right_copy(basic::right(utl_report.defout, temp + 1));
		output = utl_report.defout.substr(0, temp - 1);
	}
	utl_reportx.defout = output;
	for (i = 0; i <= 9; i++)
	{
		utl_reportx.itemgroup[i] = "";
	}
	for (i = 0; i <= 9; i++)
	{
		utl_reportx.item[i] = "";
	}
	utl_reportx.startp = 0;
	utl_reportx.endp = 0;
	utl_reportx.copies = 0;
	utl_reportx.pagelen = 66;
	utl_reportx.repwidth = 80;
	utl_reportx.repyn = utl_report.repyn;
	utl_reportx.repdate = prnt_fancydate(date_today());
	utl_reportx.printto = 0;
	if (printto != "")
	{
		utl_reportx.printto = std::stol(printto);
	}
	utl_reportx.autoscroll = 0;
	utl_reportx.printtype = utl_report.printtype;
	utl_reportx.printinit = "";
	utl_reportx.printfinish = "";
	utl_reportx.nextpage = "";
	utl_reportx.tolocal = "/027[5i";
	utl_reportx.toscreen = "/027[4i";
	utl_reportx.offset = 0;
	utl_reportx.aftertime = "";
	utl_reportx.background = "N";
}
