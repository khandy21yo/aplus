//! \file
//! \brief Report routines
//!
#ifndef _report_h_
#define _report_h_

#include "database.h"
#include "utl/utl_reportx.h"
#include "utl/utl_report.h"

static const long print_maxgrp = 30;
static const long print_maxitm = 100;

static const long OUTP_TOSPOOL = 1;	//!< Spool output
static const long OUTP_TODISPLAY = 2;	//!< Display output
static const long OUTP_TOPL = 3;	//!< PlanPerfect output
static const long OUTP_TO2020 = 4;	//!< 2020 output
static const long OUTP_TODOCUMENT = 5;	//!< Document output
static const long OUTP_TEMP = 6;	//!< Temp output
static const long OUTP_TOLOCAL = 7;	//!< Local printer output
static const long OUTP_TODEVICE = 8;	//!< Devicer output
static const long OUTP_TOWP = 9;	//!< Word Perfect output
static const long OUTP_TOFILE = 10;	//!< Text file output

class printx_cdd
{
public:
	long groups;
		//!< Number of groups
	std::vector<std::string> groupx;
		//!< Groups
	std::vector<long> groupp;
		//!< Pointer to lines
	std::vector<std::string> deflt;
		//!< Default value
	std::vector<std::string> descr;
		//!< Description
	long items;
		//!< Number of items
	std::vector<std::string> item;
		//!< items
	std::vector<std::string> sequ;
		//!< Sequences
public:
	//! Constructor
	printx_cdd(void)
	{
		groupx.resize(print_maxgrp + 1);
		groupp.resize(print_maxgrp + 1);
		deflt.resize(print_maxgrp + 1);
		descr.resize(print_maxgrp + 1);
		item.resize(print_maxitm + 1);
		sequ.resize(print_maxitm + 1);
	}
};

//
//
//
void outp_36initfromfile(
	scope_struct &scope,
	utl_reportx_cdd &utl_reportx,
	int xwidth);
long find_3printgroupitem(
	const std::string &group,
	const std::string &item,
	printx_cdd &printx);
void outp_3writestructure(
	utl_reportx_cdd &utl_reportx,
	std::ofstream &output_ch,
	printx_cdd &printx);
std::string  outp_createstr(
	const std::string &Source,
	const std::string &Item);
void outp_finish(
	utl_reportx_cdd &utl_reportx);
void outp_formff(
	utl_reportx_cdd &utl_reportx);
long outp_initform(
	utl_reportx_cdd &utl_reportx,
	const std::string &reportnum,
	const std::string &fixset);
void outp_initfromfile(
	utl_reportx_cdd &utl_reportx,
	int xwidth);
void outp_line(
	const std::string &columns,
	utl_reportx_cdd &utl_reportx,
	const std::vector<std::string> &hdr,
	const std::string &txt,
	long lineoff);
void outp_settings(
	utl_report_cdd &utl_report,
	utl_reportx_cdd &utl_reportx,
	utl_report_cdd &update_ch,
	std::string &left_side_cmd,
	std::string &right_side_cmd);
void outp_spool(
	utl_reportx_cdd &utl_reportx);

#endif
