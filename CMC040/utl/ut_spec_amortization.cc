//! \file
//! \brief Amortization Schedule
// %SBTTL "UT_SPEC_AMORTIZATION"
// %IDENT "V3.6a Calico"
//
// Source: ../../CMC030/utl/source/ut_spec_amortization.bas
// Translated from Basic to C++ using btran
// on Friday, November 24, 2017 at 23:41:57
//



#include <cstdlib>
#include <string>
#include <cstring>
#include <unistd.h>
#include <cmath>

#include "basicfun.h"
#include "pusing.h"
#include "datalist.h"

#include "preferences.h"
#include "cmcfun.h"
#include "database.h"
#include "scopedef.h"
#include "cmcfun/report.h"

#include "utl/utl_reportx.h"


//
// Function Prototypes
//

//
// Data Statement
//
static const char* DataValue[] = {
	"02","03","(01) Field  to Calc","03","03","(02) Amount of Loan","06",
	"03","(03) Interest Rate","09","03","(04) Periods Per Year","12","03",
	"(05) Total Periods","06","43","(06) Payment per Period","09","43",
	"(07) Date of First Payment","12","43","(08) Payment No.","16","35",
	"Interest      Principal    Loan Balance","0","0","", NULL};
static basic::DataListClass DataList(DataValue);

extern scope_struct scope;

//!
//! Abstract:HELP
//!	.b
//!	.lm +5
//!	This program will calculate and print (display) the amortization
//!	schedule.  There are four items which can be calculated:
//!	.table 3,25
//!	.te
//!	^*Present Value
//!	.te
//!	Interest Rate
//!	.te
//!	Number or Periods
//!	.te
//!	Payment per Period\*
//!	.end table
//!	.lm -5
//!
int main(
	int argc,
	char **argv)
{
	double amo_item;
	std::string atext;
	long cd;
	long da;
	std::string date_item;
	std::string deflt;
	double delta;
	double famo;
	std::string filename;
	long flag;
	long i;
	double iid;
	long in;
	double intrv;
	double ipp;
	long j;
	long j_item;
	long loop;
	double lpp;
	long mo;
	long nm;
	std::string n_date;
	std::string opt;
	long opt_V1;
	std::string option_item;
	std::string optlist;
	double pamo_item;
	double pd;
	double ppp;
	long py_item;
	double ra_item;
	std::string report;
	double res;
	std::string res_V2;
	long ro;
	long smg_status;
	std::string temp;
	long test;
	std::string test_date;
	std::string text;
	std::vector<std::string> title;
	long tp_item;
	std::string junk;
	BStack(20);
	OnErrorStack;

	utl_reportx_cdd utl_reportx;

	std::string flagtitle;
	std::vector<std::string> flagtype;

	long xlong;
	long ylong;
	smg_display_id smg_view;

	auto fnni = [&](long n, double i_V3)
	{
		double Result;

		Result = (pow(1 + i_V3, n) - 1) / i_V3;
		return Result;
	};

	auto fnani = [&](long n, double i_V4)
	{
		double Result;

		Result = (1 - pow(1 + i_V4, -n)) / i_V4;
		return Result;
	};
	// ++
	// Abstract:COMMAND
	//	^*AMORTIZATION\*
	//	.b
	//	.lm +5
	//	The ^*Amortization\* option calculates and prints
	//	an amortization schedule. There are four items which can be calculated:
	//	.table 3,25
	//	.te
	//	^*Present Value
	//	.te
	//	Interest Rate
	//	.te
	//	Number or Periods
	//	.te
	//	Payment per Period\*
	//	.end table
	//	^*Format: AMORTIZATION\*
	//	.b
	//	^*Example:\*
	//	.literal
	//	Menu Command Level> /AMORTIZATION
	//	.END LITERAL
	//	.lm -5
	//
	// --
	//
	// Declare constants
	//
	const long max_item = 8;
	OnErrorGoto(L_19000);
	//*******************************************************************
	// Initilize query
	//*******************************************************************
	read_initialize();
	report = "UT019";
	BGosub(setinitial);
	//
	// Create a display window
	//
	smg_status = smg$create_virtual_display(18, 78, smg_view, SMG$M_BORDER);
	//
	// Label the display
	//
	smg_status = smg$label_border(smg_view, std::string("Amortization schedule for ") + basic::edit(scope.prg_company, 140));
	smg_status = smg$paste_virtual_display(smg_view, scope.smg_pbid, 2, 2);
	//******************************************************************
L_1000:;
	// Main option menu
	//******************************************************************
	BGosub(repaint);
	//
L_1100:;
	// Enter options
	//
	scope.prg_item = "";
	optlist = "Add Change Print Help eXit";
	opt = entr_3option(scope, "COMMAND", optlist, opt_V1, 0);
	// ** Converted from a select statement **
	//
	// Control c
	//
	if (scope.scope_exit == 3)
	{
		goto L_1000;
		//
		// Exit key
		//
	}
	else if ((scope.scope_exit == SMG$K_TRM_F10) || (scope.scope_exit == SMG$K_TRM_CTRLZ))
	{
		goto exitprogram;
	}
	// ** Converted from a select statement **
	if (opt == "A")
	{
		//*****************************************************
		// Add new information on the screen
		//*****************************************************
		BGosub(setinitial);
		flag = 1;
		for (loop = 1; loop <= max_item; loop++)
		{
			BGosub(dataentry);
		}
		for (loop = 1; loop <= max_item; loop++)
		{
			// ** Converted from a select statement **
			//
			// Control c
			//
			if (scope.scope_exit == 3)
			{
				goto L_1000;
				//
				// Exit key
				//
			}
			else if ((scope.scope_exit == SMG$K_TRM_F10) || (scope.scope_exit == SMG$K_TRM_CTRLZ))
			{
				goto L_1100;
			}
add:;
			flag = 0;
			BGosub(dataentry);
			// ** Converted from a select statement **
			//
			// Control c
			//
			if (scope.scope_exit == 3)
			{
				goto L_1000;
				//
				// Uparrow
				//
			}
			else if (scope.scope_exit == SMG$K_TRM_UP)
			{
				if (loop > 1)
				{
					loop = loop - 1;
				}
				goto add;
				//
				// SMG$K_TRM_DOWN
				//
			}
			else if (scope.scope_exit == SMG$K_TRM_DOWN)
			{
				if (loop < max_item)
				{
					loop = loop + 1;
				}
				goto add;
				//
				// Exit key
				//
			}
			else if ((scope.scope_exit == SMG$K_TRM_F10) || (scope.scope_exit == SMG$K_TRM_CTRLZ))
			{
				goto L_1100;
			}
		}
		BGosub(calcul);
	}
	else if (opt == "C")
	{
changer:;
		//*****************************************************
		// Change information on the screen
		//*****************************************************
		loop = entr_3number(scope, scope.smg_option, "",
			"Item to change", 0.0, 4, "##", junk);
		// ** Converted from a select statement **
		//
		// Control c
		//
		if (scope.scope_exit == 3)
		{
			goto L_1000;
			//
			// Exit key
			//
		}
		else if ((scope.scope_exit == SMG$K_TRM_F10) || (scope.scope_exit == SMG$K_TRM_CTRLZ))
		{
			goto L_1100;
		}
		if (loop == 0)
		{
			BGosub(calcul);
			goto L_1100;
		}
		if (loop < 1 || loop > max_item)
		{
			goto changer;
		}
changer1:;
		flag = 0;
		BGosub(dataentry);
		// ** Converted from a select statement **
		//
		// Control c
		//
		if (scope.scope_exit == 3)
		{
			goto L_1000;
			//
			// Uparrow
			//
		}
		else if (scope.scope_exit == SMG$K_TRM_UP)
		{
			if (loop > 1)
			{
				loop = loop - 1;
			}
			goto changer1;
			//
			// SMG$K_TRM_DOWN
			//
		}
		else if (scope.scope_exit == SMG$K_TRM_DOWN)
		{
			if (loop < max_item)
			{
				loop = loop + 1;
			}
			goto changer1;
			//
			// Exit key
			//
		}
		else if ((scope.scope_exit == SMG$K_TRM_F10) || (scope.scope_exit == SMG$K_TRM_CTRLZ))
		{
			goto L_1100;
		}
		goto changer;
	}
	else if (opt == "P")
	{
		//*****************************************************
		// Print
		//*****************************************************
		entr_3message(scope, "", 1 + 16);
		//******************************************************************
		// Set up the report settings screen
		//******************************************************************
		if (outp_initform(utl_reportx, report, "") != CMC$_NORMAL)
		{
			goto L_1000;
		}
		if (utl_reportx.stat)
		{
			goto L_4500;
		}
		//
		// Title
		//
		title[1] = "AMORTIZATION  SCHEDULE";
		title[2] = "";
		title[3] = ".";
		text = std::string("Amount of Loan($):") + basic::Format(amo_item, "###,###,###.##") + std::string(24, ' ') + "Period per Year : " + basic::Format(py_item, "###");
		outp_line("", utl_reportx, title, text, 0);
		text = std::string("Future Value ($) :") + basic::Format(famo, "###,###,###.##") + std::string(24, ' ') + "Total Periods   : " + basic::Format(tp_item, "###");
		outp_line("", utl_reportx, title, text, 0);
		text = std::string("Period Payment($):") + basic::Format(pamo_item, "###,###,###.##") + std::string(24, ' ') + "Interest Rate(%): " + basic::Format(ra_item, " ##.##");
		outp_line("", utl_reportx, title, text, 0);
		outp_line("", utl_reportx, title, "", 0);
		if (utl_reportx.stat)
		{
			goto L_4500;
		}
		title[3] = "Date            Interest     Principal""   BalanceLoan   InterestPTD  PrincipalPTD";
		title[4] = ".";
		text = title[3];
		outp_line("", utl_reportx, title, text, 0);
		outp_line("", utl_reportx, title, "", 0);
		n_date = date_item;
		iid = pd = 0.0;
		if (boost::trim_right_copy(date_item) == "")
		{
			for (j = 0; j <= tp_item - 1; j++)
			{
				BGosub(repsub);
				text = basic::Format(j + 1, "##########") + basic::Format(ipp, "###,###,###.##") + basic::Format(ppp, "###,###,###.##") + basic::Format(lpp, "###,###,###.##") + basic::Format(iid, "###,###,###.##") + basic::Format(pd, "###,###,###.##");
				outp_line("", utl_reportx, title, text, 0);
				if (utl_reportx.stat)
				{
					goto L_4500;
				}
			}
			goto L_4500;
		}
		if (floor(12.0 / py_item) == 12.0 / py_item && py_item != 0)
		{
			nm = 12 / py_item;
			da = std::stol(basic::right(n_date, 7));
			mo = std::stol(basic::mid(n_date, 5, 2));
			ro = std::stol(n_date.substr(0, 4));
			for (j = 0; j <= tp_item - 1; j++)
			{
				BGosub(repsub);
				text = prnt_date(n_date, 8) +
					basic::Format(ipp, "###,###,###.##") +
					basic::Format(ppp, "###,###,###.##") +
					basic::Format(lpp, "###,###,###.##") +
					basic::Format(iid, "###,###,###.##") +
					basic::Format(pd, "###,###,###.##");
				outp_line("", utl_reportx, title, text, 0);
				if (utl_reportx.stat)
				{
					goto L_4500;
				}
				if (mo + nm > 12)
				{
					ro = ro + 1;
				}
				mo = mo + nm - floor((mo + nm - 1) / 12) * 12;
				n_date = test_date = basic::Format(ro, "<0>###") + basic::Format(mo, "<0>#") + basic::Format(da, "<0>#");
				test = 0;
				while (date_invdcode(date_daycode(n_date)) != n_date || std::stol(basic::mid(n_date, 5, 2)) != mo)
				{
					test = test + 1;
					n_date = date_invdcode(date_daycode(test_date) - test);
				}
				if (utl_reportx.stat)
				{
					goto L_4500;
				}
			}
			goto L_4500;
		}
		if (py_item == 52)
		{
			cd = date_daycode(date_item);
			for (j = 0; j <= tp_item - 1; j++)
			{
				BGosub(repsub);
				text = prnt_date(date_invdcode(cd + 7 * j), 8) +
					basic::Format(ipp, "###,###,###.##") +
					basic::Format(ppp, "###,###,###.##") +
					basic::Format(lpp, "###,###,###.##") +
					basic::Format(iid, "###,###,###.##") +
					basic::Format(pd, "###,###,###.##");
				outp_line("", utl_reportx, title, text, 0);
				if (utl_reportx.stat)
				{
					goto L_4500;
				}
			}
			goto L_4500;
		}
		intrv = 0.0;
		if (py_item != 0)
		{
			intrv = 366.0 / py_item;
		}
		ro = std::stol(date_item.substr(0, 4));
		j = 0;
nextyear:;
		cd = date_daycode(n_date);
		for (i = 0; i <= py_item - 1; i++)
		{
			in = i * intrv;
			BGosub(repsub);
			text = prnt_date(date_invdcode(cd + in), 8) +
				basic::Format(ipp, "###,###,###.##") +
				basic::Format(ppp, "###,###,###.##") +
				basic::Format(lpp, "###,###,###.##") +
				basic::Format(iid, "###,###,###.##") +
				basic::Format(pd, "###,###,###.##");
			outp_line("", utl_reportx, title, text, 0);
			j = j + 1;
			if (j == tp_item)
			{
				goto L_4500;
			}
		}
		ro = ro + 1;
		n_date = basic::Format(ro, "####") + basic::right(n_date, 5);
		goto nextyear;
		//
L_4500:;
		outp_finish(utl_reportx);
		goto L_1000;
	}
	else if (opt == "H")
	{
		help_34message(scope, "", "H", scope.prg_program, "", "HELP");
		goto L_1000;
	}
	else if (opt == "X")
	{
		goto exitprogram;
	}
	goto L_1100;
exitprogram:;
	//******************************************************************
	// Exit the program
	//******************************************************************
	smg_status = smg$delete_virtual_display(smg_view);
	subr_3exitprogram(scope, "", "");
setinitial:;
	//******************************************************************
	// Set initial value
	//******************************************************************
	flagtitle = "Item   Description";
	flagtype.resize(6);
	flagtype[0] = "04";
	flagtype[1] = "02   Present Value";
	flagtype[2] = "03   Interest  Rate";
	flagtype[3] = "05   Total Periods";
	flagtype[4] = "06   Pay Per Period";
	option_item = "06";
	amo_item = 0.0;
	ra_item = 0.0;
	py_item = 0;
	tp_item = 0;
	pamo_item = 0.0;
	j_item = 0.0;
	date_item = "        ";
	BReturn;

repaint:;
	//******************************************************************
	// Repaint the screen
	//******************************************************************
	DataList.Reset();
	DataList.Read(xlong);
	DataList.Read(ylong);
	DataList.Read(atext);
	while (xlong)
	{
		smg_status = smg$put_chars(smg_view, atext, xlong, ylong);
		DataList.Read(xlong);
		DataList.Read(ylong);
		DataList.Read(atext);
	}
	flag = 1;
	for (loop = 1; loop <= max_item; loop++)
	{
		BGosub(dataentry);
	}
	BReturn;

dataentry:;
	//******************************************************************
	// Enter/Diaplay items
	//******************************************************************
	temp = boost::trim_right_copy(scope.prg_item);
	scope.prg_item = std::string("FLD") + basic::Format(loop, "<0>##");
	switch (loop)
	{
	case 1:

		// ++
		// Abstract:FLD001
		//	.x Field
		//	^*(01) Field\*
		//	.b
		//	.lm +5
		//	The ^*Field\* enters one of four
		//	codes indicating which item is to be calculated.
		//	.b
		//	Valid codes may be displayed by pressing ^*List Choices\*.
		//	.lm -5
		//
		// --
		option_item = basic::edit(entr_3stringlist(scope, smg_view, "2;15", "Option ", option_item, flag, "'E", deflt, flagtype, flagtitle, "007"), -1);
		//**** Fix? ****
		j = 0;
		for (i = 1; i <= 4; i++)
		{
			if (flagtype[i].substr(0, 2) == option_item)
			{
				j = i;
			}
		}
		smg_status = smg$put_chars(smg_view, basic::right(flagtype[j], 3), 2, 18, 0, SMG$M_BOLD);
		break;

	case 2:

		// ++
		// Abstract:FLD002
		//	^*(02) Amount of Loan\*
		//	.b
		//	.lm +5
		//	The ^*Amount of Loan\* field enters
		//	the amount of an obligation.
		//	.b
		//	This field will accommodate a number as large as 99,999,999.99.
		//	.lm -5
		//
		// --
		amo_item = entr_3number(scope, smg_view, "4;7", "Loan ",
			amo_item, flag, "###,###,###.##", deflt);
		break;

	case 3:

		// ++
		// Abstract:FLD003
		//	^*(03) Interest Rate\*
		//	.b
		//	.lm +5
		//	The ^*Interest Rate\* field enters
		//	a periodic interest rate.
		//	.b
		//	If the periodic interest rate is to be the same as
		//	an effective interest rate, there must be only one period
		//	per year.
		//	.lm -5
		//
		// --
		ra_item = entr_3number(scope, smg_view, "7;13",
			"Rate ", ra_item, flag, "##.##", deflt);
		break;

	case 4:

		// ++
		// Abstract:FLD004
		//	^*(04) Period per Year\*
		//	.b
		//	.lm +5
		//	The ^*Period per Year\* field enters
		//	the number of periods there will be per year.
		//	.b
		//	For example, if the present value will be compounded monthly,
		//	enter 12 periods per year.
		//	.lm -5
		//
		// --
		py_item = entr_3number(scope, smg_view, "10;14",
			"Periods ", py_item * 1.0, flag, "###", deflt);
		break;

	case 5:

		// ++
		// Abstract:FLD005
		//	^*(05) Total Periods\*
		//	.b
		//	.lm +5
		//	The ^*Total Periods\* field enters
		//	the number of periods needed to pay a loan.
		//	.lm -5
		//
		// --
		tp_item = entr_3number(scope, smg_view, "13;14",
			"Periods ", tp_item * 1.0, flag, "###", deflt);
		break;

	case 6:

		// ++
		// Abstract:FLD006
		//	^*(06) Payment per Period\*
		//	.b
		//	.lm +5
		//	The ^*Payment per Period\* field enters
		//	the amount required to be paid each period.
		//	.lm -5
		//
		// --
		pamo_item = entr_3number(scope, smg_view, "7;47",
			"Payment ", pamo_item, flag, "###,###,###.##", deflt);
		break;

	case 7:

		// ++
		// Abstract:FLD007
		//	.x Starting Date
		//	^*(07) Date of the First Payment\*
		//	.b
		//	.lm +5
		//	The ^*Date of the First Payment\* field
		//	enters the date the first payment is due.
		//	.lm -5
		//
		// --
		date_item = entr_3date(scope, smg_view, "10;50", "From Date", date_item, flag, "'E", deflt, 8);
		break;

	case 8:

		// ++
		// Abstract:FLD008
		//	^*(08) Payment Number\*
		//	.b
		//	.lm +5
		//	The ^*Payment Number\* field
		//	enters the payment which is displayed on the
		//	screen.
		//	.lm -5
		//
		// --
		j_item = entr_3number(scope, smg_view, "13;54",
			"Number ", j_item * 1.0, flag, "###", deflt);
		break;

	}
	scope.prg_item = temp;
	BReturn;

calcul:;
	flag = 1;
	//
	// Ignore errors (usually caused by fields not filled in)
	//
	try
	{
		// ** Converted from a select statement **
		if (option_item == "02")
		{
			if (py_item != 0)
			{
				amo_item = pamo_item * fnani(tp_item, 0.01 * ra_item / py_item);
			}
			loop = 2;
			BGosub(dataentry);
		}
		else if (option_item == "03")
		{
			ra_item = 0.0008;
			delta = 0.0008;
			if (amo_item != 0.0)
			{
				delta = pamo_item / amo_item * (pow(1.0 + ra_item, tp_item) - 1.0) / pow(1.0 + ra_item, tp_item);
			}
			while (fabs(ra_item - delta) > 0.000001)
			{
				ra_item = delta;
				delta = pamo_item / amo_item * (pow(1.0 + ra_item, tp_item) - 1.0) / pow(1.0 + ra_item, tp_item);
			}
			ra_item = delta * py_item * 100.0;
			loop = 3;
			BGosub(dataentry);
		}
		else if (option_item == "05")
		{
			if (py_item != 0)
			{
				tp_item = -log(1 - amo_item * ra_item * 0.01 / py_item / pamo_item) / log(1 + ra_item * 0.01 / py_item) + 1;
			}
			loop = 5;
			BGosub(dataentry);
			res = 0.0;
			if (py_item != 0)
			{
				res = amo_item * pow(1 + ra_item * 0.01 / py_item, tp_item) - pamo_item * fnni(tp_item - 1, ra_item * 0.01 / py_item) * (1 + ra_item * 0.01 / py_item);
			}
			res_V2 = basic::Format(res, "$###,###,###.##");
			smg_status = smg$put_chars(smg_view, res_V2, 14, 3);
			smg_status = smg$put_chars(smg_view, "( Last Payment )", 15, 3);
		}
		else if (option_item == "06")
		{
			if (py_item != 0)
			{
				pamo_item = amo_item / fnani(tp_item, ra_item * 0.01 / py_item);
			}
			loop = 6;
			BGosub(dataentry);
		}
	}
	catch(basic::BasicError &Be)
	{
		goto L_10360;
	}
L_10360:;
	j = j_item;
	j = j - 1;
	BGosub(repsub);
	text = basic::Format(ipp, "##,###,###.##") + "  " + basic::Format(ppp, "##,###,###.##") + "  " + basic::Format(lpp, "###,###,###.##");
	smg_status = smg$put_chars(smg_view, text, 17, 31, 0, SMG$M_BOLD);
	BReturn;

repsub:;
	if (ra_item != 0.0)
	{
		if (j == -1)
		{
			ipp = ppp = 0.0;
			lpp = amo_item;
			goto retrepsub;
		}
		ipp = (func_round(amo_item, 2) * pow(1 + ra_item * 0.01 / py_item, j) - func_round(pamo_item, 2) * fnni(j, ra_item * 0.01 / py_item)) * ra_item * 0.01 / py_item;
		ppp = func_round(pamo_item, 2) - func_round(ipp, 2);
		lpp = amo_item * pow(1 + ra_item * 0.01 / py_item, j) - func_round(pamo_item, 2) * fnni(j, ra_item * 0.01 / py_item) - ppp;
		iid = iid + ipp;
		pd = pd + ppp;
	}
retrepsub:;
	BReturn;

helperror:;
	//******************************************************************
	// Help Message for an error
	//******************************************************************
	help_34message(scope, std::to_string(0) + " " + basic::ert(0), "E", "", filename, std::to_string(0));
	goto exitprogram;
	//******************************************************************
L_19000:;
	// Error trapping
	//******************************************************************
	filename = "";
	OnErrorZero;
	goto helperror;

	return EXIT_SUCCESS;
}
