/*
 * File Layout for: UTL.UTL_REPORT on 21-May-01
 *
 * Utility Report File
 */
#ifndef _utl_sysrep_h_
#define _utl_sysrep_h_

#include "utl/utl_report.h"

//!
//! \brief Utility Report File
//!
//! This version is the master copt, which has the same layout as
//! utl_report_cdd, only the table name is different..
//!
class utl_sysrep_cdd : public utl_report_cdd
{
public:
	utl_sysrep_cdd() : utl_report_cdd()
	{
		table_name ="utl_sysrep";
	}
};

#endif
