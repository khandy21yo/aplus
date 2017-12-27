//! \file
//!
//! \brief CMC Utility Set File
//!
#ifndef _utl_set_h_
#define _utl_set_h_

#include "database.h"

//! \brief CMC Utility Set File
//!
class utl_set_cdd : public db_rmsindexed_cdd
{
public:
	//!
	//! \brief Constructor
	//!
	utl_set_cdd(void)
	{
		table_name = "utl_set";
		fields["programname"] =
			db_field_string_cdd(programname, 39);
		fields["item"] =
			db_field_string_cdd(item, 6);
		fields["system"] =
			db_field_string_cdd(system, 2);
		fields["allowund"] =
			db_field_string_cdd(allowund, 1);
		fields["hard"] =
			db_field_string_cdd(hard, 1);
		fields["sdata"] =
			db_field_string_cdd(sdata, 30);
		fields["fdata"] =
			db_field_string_cdd(fdata, 30);
	}

public:
	//! Program name
	std::string programname; //[39];
	//! Item number
	std::string item; //[6];
	//! System name
	std::string system; //[2];
	//! Yes or No Flag for Undefined Input
	std::string allowund; //[1];
	//!  Hard/Soft/Field default
	std::string hard; //[1];
	//! Data
	std::string sdata; //[30];
	//! Data Format
	std::string fdata; //[30];
};

//
// Prototypes
//
long read_35set(
	std::string group,
	std::string item,
	utl_set_cdd &utl_set_read);

#endif
