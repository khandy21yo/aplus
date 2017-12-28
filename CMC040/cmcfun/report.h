//! \file
//! \brief Report routines
//!
#ifndef _report_h_
#define _report_h_

#include "utl/utl_reportx.h"

static const long print_maxgrp = 30;
static const long print_maxitm = 100;

static const long OUTP_TOSPOOL = 1;	//!< Spool output
static const long OUTP_TODISPLAY = 2;	//!< Display output

class printx_cdd
{
public:
	long print_groups;
		//!< Number of groups
	std::string groupx[print_maxgrp + 1];
		//!< Groups
	long groupp[print_maxgrp + 1];
		//!< Pointer to lines
	std::string deflt[print_maxgrp + 1];
		//!< Default value
	std::string _descr[print_maxgrp + 1];
		//!< Description
	long items;
		//!< Number of items
	std::string item[print_maxitm + 1];
		//!< items
	std::string sequ[print_maxitm + 1];
		//!< Sequences
};

//
//
//
void outp_3writestructure(
	utl_reportx_cdd &utl_reportx,
	std::ofstream &output_ch,
	printx_cdd &printx);
long outp_initform(
	utl_reportx_cdd &utl_reportx,
	const std::string &reportnum,
	const std::string &fixset);
void outp_initfromfile(
	utl_reportx_cdd utl_reportx,
	int xwidth);
void outp_line(
	const std::string &columns,
	utl_reportx_cdd &utl_reportx,
	const std::vector<std::string> &hdr,
	const std::string &txt,
	long lineoff);

#endif
