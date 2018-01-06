//! \file
//! \brief Write Report Info into File
// %SBTTL "OUTP_3WRITESTRUCTURE"
// %IDENT "V3.6a Calico"


//
// Source: ../../CMC030/cmcfun/source/outp_3writestructure.bas
// Translated from Basic to C++ using btran
// on Wednesday, December 27, 2017 at 16:33:18
//

#include <iostream>
#include <fstream>
#include <string>
#include <cstdlib>
#include <cstring>
#include <unistd.h>
#include <cstring>
#include <ctime>
#include "basicfun.h"

#include "preferences.h"
#include "cmcfun.h"
#include "scopedef.h"
#include "database.h"

#include "smg/smg.h"
#include "cmcfun/report.h"
#include "utl/utl_reportx.h"

//
//! \brief Write Report Info into File
//!
//!	Writes out the UTL_REPORTX structure in a format that
//!	OUTP_INITFROMFILE can read.
//!
//! \returns This subroutine writes a report's information into a
//!	file.
//!
//! \author 10/20/92 - Kevin Handy
//!
void outp_3writestructure(
	utl_reportx_cdd &utl_reportx,
		//!> The passed definition of the current report file.
	std::ofstream &output_ch,
		//!> The returned output file to write the report to.
	printx_cdd &printx)
		//!< Printer definition structure
{
	std::string finup;
	std::string intro;
	long item;
	std::string jj;
	long loop;
	long smg_status;

	std::ofstream spool_ch;

	//**************************************************************
	// GET A UNIQUE SPOOLER FILENAME
	//**************************************************************

	auto fnspool = [&](std::string qjn, std::ofstream &chn)
	{
		std::string Result;
		std::string sfile;

		srand(time(0));
L_30110:;
		sfile = std::string("/tmp/S") + qjn + "_" +
			std::to_string(random()) + ".spl";
		try
		{
			chn.open(sfile);
			if (!chn.is_open()) { throw basic::BasicError(5); }
		}
		catch(basic::BasicError &Be)
		{
			if (Be.err == 5)
			{
				goto L_30120;
			}
			if (Be.err == 138)
			{
				goto L_30113;
			}
			std::cout << std::string("XXX: ERROR# ") +
				std::to_string(Be.err) << std::endl;
			goto L_30113;
		}
		chn.close();
L_30113:;
		goto L_30110;
		//
L_30120:;
		// Create the file to allocate it to this job
		//
		Result = sfile + "";
		return Result;
	};

	//***************************************************************
	// TEMPORARY PRINT COMMAND FILE
	//***************************************************************
	jj = read_sysjob();
	// ** Converted from a select statement **
	// Spool
	if (utl_reportx.printto == OUTP_TOSPOOL)
	{
		utl_reportx.defout = fnspool(jj, spool_ch);
		spool_ch.open(utl_reportx.defout);
		if (!spool_ch.is_open()) { throw basic::BasicError(5); }
		spool_ch.close();
		goto L_4175;
	}
	//
L_4175:;
	// Dump out Report Settings print file
	//
	output_ch << "RN>" << utl_reportx.repnum << std::endl;
	output_ch << "PG>" << boost::trim_right_copy(utl_reportx.prodev) + boost::trim_right_copy(utl_reportx.pronam) << std::endl;
	output_ch << "SP>" << utl_reportx.startp << std::endl;
	output_ch << "EP>" << utl_reportx.endp << std::endl;
	output_ch << "PD>" << utl_reportx.repyn << std::endl;
	output_ch << "RD>" << utl_reportx.repdate << std::endl;
	output_ch << "AS>" << utl_reportx.autoscroll << std::endl;
	output_ch << "OD>" << utl_reportx.defout << std::endl;
	output_ch << "OF>" << utl_reportx.offset << std::endl;
	output_ch << "AF>" << utl_reportx.aftertime << std::endl;
	output_ch << "BG>" << utl_reportx.background << std::endl;
	output_ch << "XX>" << utl_reportx.printto << std::endl;
	output_ch << "U1>" << utl_reportx.optdef[0] << std::endl;
	output_ch << "U2>" << utl_reportx.optdef[1] << std::endl;
	output_ch << "U3>" << utl_reportx.optdef[2] << std::endl;
	output_ch << "U4>" << utl_reportx.optdef[3] << std::endl;
	output_ch << "U5>" << utl_reportx.optdef[4] << std::endl;
	output_ch << "U6>" << utl_reportx.optdef[5] << std::endl;
	output_ch << "U7>" << utl_reportx.optdef[6] << std::endl;
	output_ch << "U8>" << utl_reportx.optdef[7] << std::endl;
	output_ch << "U9>" << utl_reportx.optdef[8] << std::endl;
	output_ch << "U0>" << utl_reportx.optdef[9] << std::endl;
	output_ch << "TL>" << utl_reportx.tolocal << std::endl;
	output_ch << "TS>" << utl_reportx.toscreen << std::endl;
	//
	// Spooler stuff
	//
	if (utl_reportx.printto == OUTP_TOSPOOL)
	{
		output_ch << "CP>" << utl_reportx.copies << std::endl;
		output_ch << "SL>" << utl_reportx.spool << std::endl;
		output_ch << "SF>" << utl_reportx.spoolform << std::endl;
	}
	//
	// Use printer stuff only if it makes sense
	//
	if ((utl_reportx.printto != OUTP_TODISPLAY) && (utl_reportx.printto < 10))
	{
		//
		// Create initilizier string
		//
		intro = "";
		finup = "";
		loop = find_3printgroupitem("II", "*", printx);
		if (loop > 0)
		{
			intro = outp_createstr(printx.sequ[loop], "*");
		}
		loop = find_3printgroupitem("ZZ", "*", printx);
		if (loop > 0)
		{
			finup = outp_createstr(printx.sequ[loop], "*");
		}
		for (loop = 1; loop <= printx.groups; loop++)
		{
			if (find_3printgroupitem(printx.groupx[loop], "*", printx) == 0 || printx.groupx[loop] == "LP")
			{
				item = find_3printgroupitem(printx.groupx[loop], printx.deflt[loop], printx);
				if (item)
				{
					intro = intro + outp_createstr(printx.sequ[item], printx.deflt[loop]);
				}
			}
		}
		//
		// Printer control strings
		//
		output_ch << "PT>" << utl_reportx.printtype << std::endl;
		output_ch << "PC>" << intro << std::endl;
		output_ch << "ZZ>" << finup << std::endl;
		for (loop = 1; loop <= printx.groups; loop++)
		{
			if (find_3printgroupitem(printx.groupx[loop], "*", printx) == 0 || printx.groupx[loop] == "LP")
			{
				output_ch << printx.groupx[loop] << ">" << boost::trim_right_copy(printx.deflt[loop]) << std::endl;
			}
		}
		//
		// Next page stuff
		//
		loop = find_3printgroupitem("NP", "*", printx);
		if (loop > 0)
		{
			output_ch << "NP>" << outp_createstr(printx.sequ[loop], "*") << std::endl;
		}
	}
	return;
}
