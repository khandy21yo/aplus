//! \file
//! \brief System report table

#ifndef _utl_report_sys_h_
#define _utl_report_sys_h_

#include "utl/utl_report.h"

class utl_report_sys_cdd : public utl_report_cdd
{
public:
	utl_report_sys_cdd(void) : utl_report_cdd()
	{
		table_name = "utl_report_sys";
	}
	utl_report_sys_cdd & operator = (const utl_report_cdd &record)
	{
		utl_report_cdd::operator=(record);
		return *this;
	}
};

#endif
