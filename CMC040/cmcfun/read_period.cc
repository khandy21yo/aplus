//! \file
//! \brief Read Period File
// %SBTTL "READ_PERIOD"
// %IDENT "V3.6a Calico"
//
// Source: ../../CMC030/cmcfun/source/read_period.bas
// Translated from Basic to C++ using btran
// on Thursday, January 18, 2018 at 16:27:09
//

#include <cstdlib>
#include <cstring>
#include <unistd.h>
#include "basicfun.h"
#include "basicchannel.h"

#include "preferences.h"
#include "cmcfun.h"
#include "scopedef.h"
#include "database.h"
#if 0
//! \todo create this include file
#include "utl/utl_period.h"
#endif


extern scope_struct scope;

//!
//! Abstract:HELP
//!	.lm +5
//!	.b
//!	This function reads the Period file.
//!	.lm -5
//!
//! Parameters:
//!
//!	XOPTION$
//!	.table
//!		READ  Read period status
//!		FIND  Search for a period
//!		DATE  Search for period by XSTART_DATE$
//!	.endtable
//!
//!	XERA$
//!
//! Return Values:
//!
//!	XPERIOD$
//!
//!	XPER_DESC$
//!		String that is the period description.
//!
//!	XSTATUS$
//!		String that returns the quarter.
//!	.table
//!		'1' 1st quarter
//!		'2' 2nd quarter
//!		'3' 3rd quarter
//!		'4' 4th quarter
//!	.endtable
//!
//!	XSTART_DATE$
//!		String that is the beginning date.
//!
//!	XFINISH_DATE$
//!		String that is the last date.
//!
//!	XAGE%
//!		When used in FIND, the number of periods to skip
//!		after the searched period.
//!		Else returns the age from the record.
//!
//! Author:
//!
//!	12/18/87 - Frank Starman
//!
//! --
long read_period(
	const std::string &xoption,
	const std::string &xera,
	std::string &xperiod,
	std::string &xper_desc,
	std::string &xstatus,
	std::string &xstart_date,
	std::string &xfinish_date,
	long &xage)
{
#if 1
	abort();
//! \todo Actually convert this file
#else
	long Result;
	std::string filename;
	long per_xage;
	std::string search_age;
	std::string temp_xperiod;
	long utl_period_ch;

	OnErrorStack;
	long utl_period_ch;
	utl_period_cdd utl_period;
	//
	// Initialize
	//
	Result = 0;
	xstatus = "?";
	temp_xperiod = xperiod;
	xper_desc = basic::Qstring(utl_period.description.size(), '?');
	if (xoption != "DATE")
	{
		xstart_date = "00000000";
		xfinish_date = "00000000";
	}
	// ** Converted from a select statement **
	{
		auto TempS = xoption
		if (TempS == "READ")
		{
			//
			// Read info about the period
			//
			try
			{
				if (basic::edit(xera, -1) != "")
				{
					BasicChannel[utl_period_ch].SetKey(0);
					BasicChannel[utl_period_ch].SetKeyMode(Equal);
					BasicChannel[utl_period_ch].SetKeyValue(xera + xperiod);
					BasicChannel[utl_period_ch].SetRegardless();
					BasicChannel[utl_period_ch].Get();
				}
				else
				{
					BasicChannel[utl_period_ch].SetKey(1);
					BasicChannel[utl_period_ch].SetKeyMode(Equal);
					BasicChannel[utl_period_ch].SetKeyValue(xperiod);
					BasicChannel[utl_period_ch].SetRegardless();
					BasicChannel[utl_period_ch].Get();
				}
			}
			catch(basic::BasicError &Be)
			{
				if (Be.err == 155)
				{
					Result = 1;
					goto exitfunction;
				}
				filename = "UTL_PERIOD";
				goto helperror;
			}
			xper_desc = utl_period.description;
			xstart_date = utl_period.beg_date;
			xfinish_date = utl_period.end_date;
			xstatus = utl_period.period_status;
			xage = std::stol(utl_period.age);
		}
		else if (TempS == "FIND")
		{
			//
			// Find period and return info about it
			//
			temp_xperiod = basic::Qstring(utl_period.year.size() + utl_period.cycle.size(), '?');
			if (basic::edit(xera, -1) != "")
			{
				if (boost::trim_right_copy(xperiod) != "")
				{
					try
					{
						BasicChannel[utl_period_ch].SetKey(0);
						BasicChannel[utl_period_ch].SetKeyMode(Equal);
						BasicChannel[utl_period_ch].SetKeyValue(xera + xperiod);
						BasicChannel[utl_period_ch].SetRegardless();
						BasicChannel[utl_period_ch].Get();
					}
					catch(basic::BasicError &Be)
					{
						if (Be.err == 5 || Be.err == 9 || Be.err == 155)
						{
							Result = 1;
							goto exitfunction;
						}
						filename = "UTL_PERIOD";
						goto helperror;
					}
					per_xage = std::stol(utl_period.age);
				}
				else
				{
					per_xage = 0;
				}
				search_age = basic::Format(per_xage + xage, "<0>###");
				try
				{
					BasicChannel[utl_period_ch].SetKey(2);
					BasicChannel[utl_period_ch].SetKeyMode(Equal);
					BasicChannel[utl_period_ch].SetKeyValue(xera + search_age);
					BasicChannel[utl_period_ch].SetRegardless();
					BasicChannel[utl_period_ch].Get();
				}
				catch(basic::BasicError &Be)
				{
					if (Be.err == 155)
					{
						Result = 1;
						goto exitfunction;
					}
					filename = "UTL_PERIOD";
					goto helperror;
				}
				temp_xperiod = utl_period.year + utl_period.cycle;
				xper_desc = utl_period.description;
				xstatus = utl_period.period_status;
				xstart_date = utl_period.beg_date;
				xfinish_date = utl_period.end_date;
			}
		}
		else if (TempS == "DATE")
		{
			//
			// Find a period for the era that contains the entered
			// date XSTART_DATE$
			//
			if (basic::edit(xera, -1) != "")
			{
				try
				{
					BasicChannel[utl_period_ch].SetKey(0);
					BasicChannel[utl_period_ch].SetKeyMode(Equal);
					BasicChannel[utl_period_ch].SetKeyValue(xera);
					BasicChannel[utl_period_ch].SetRegardless();
					BasicChannel[utl_period_ch]].Find()
				}
				catch(basic::BasicError &Be)
				{
					filename = "UTL_PERIOD";
					goto helperror;
				}
			}
			else
			{
				Result = 1;
				goto exitfunction;
			}
L_1600:;
			try
			{
				BasicChannel[utl_period_ch].SetRegardless();
				BasicChannel[utl_period_ch].Get();
			}
			catch(basic::BasicError &Be)
			{
				if (Be.err == 11)
				{
					Result = 1;
					goto exitfunction;
				}
				filename = "UTL_PERIOD";
				goto helperror;
			}
			if (utl_period.era != xera)
			{
				Result = 1;
				goto exitfunction;
			}
			if (utl_period.end_date < xstart_date)
			{
				goto L_1600;
			}
			if (utl_period.beg_date > xstart_date)
			{
				Result = 1;
				goto exitfunction;
			}
			else
			{
				temp_xperiod = utl_period.year + utl_period.cycle;
				xper_desc = utl_period.description;
				xstart_date = utl_period.beg_date;
				xfinish_date = utl_period.end_date;
				xstatus = utl_period.period_status;
				xage = std::stol(utl_period.age);
			}
		}
	}
exitfunction:;
	xperiod = temp_xperiod;
	return Result;
helperror:;
	//******************************************************************
	// Help Message for an error
	//******************************************************************
	help_34message(scope, std::to_string(Be.erl) + " " + basic::ert(Be.err), "E", Be.ern, filename, std::to_string(Be.err));
	assg_freechannel(utl_period_ch);
	Result = 1;
	goto exitfunction;
#endif
}
