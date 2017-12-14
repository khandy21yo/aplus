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
	}

	//!
	//! \brief Copy into a db_map_cdd into specific class variables
	//!
	virtual void copy_tomap(db_map_cdd &dbmap)
	{
		dbmap["programname"] = programname;
		dbmap["item"] = item;
		dbmap["system"] = system;
		dbmap["allowund"] = allowund;
		dbmap["hard"] = hard;
		dbmap["sdata"] = sdata;
		dbmap["fdata"] = fdata;
	}
	//!
	//! \brief Copy into a db_map_cdd into specific class variables
	//!
	virtual void copy_frommap(db_map_cdd &dbmap)
	{
		programname = dbmap["programname"];
		item = dbmap["item"];
		system = dbmap["system"];
		allowund = dbmap["allowund"];
		hard = dbmap["hard"];
		sdata = dbmap["sdata"];
		fdata = dbmap["fdata"];
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
