//! \file
//! \brief Report routines
//!
#ifndef _report_h_
#define _report_h_

long outp_initform(
	utl_reportx_cdd utl_reportx,
	const std::string &reportnum,
	const std::string &fixset);
void outp_line(
	const std::string &columns,
	utl_reportx_cdd &utl_reportx,
	const std::vector<std::string> &hdr,
	const std::string &txt,
	long lineoff);

#endif
