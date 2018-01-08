//! \file
//! \brief open library
//!
//! \author Kevin Handy, Dec 2017

#include <iostream>

#include "preferences.h"
#include "database.h"
#include "smg/lbr.h"

//!
//! \brief Open library
//!
long lbr$open(
	lbr_index_cdd &lr_index,	//!< lbr indes structure
	const std::string &lib1_name,	//!< Library to open
	long a,				//!< ???
	const std::string &extension)	//!< Library extension
{
	PGconn *dbh;		// Database connection
	bool testflag;		// status flag

	//
	// Save the name
	//
	lr_index.name = lib1_name;

	//
	// Does it exist
	//
	dbh = db_conn.get_dbh();

	std::string cmd =
		"SELECT * FROM information_schema.tables WHERE table_name = '" +
		lib1_name +
		"'";

//	std::cerr << "Check for lib " << cmd << std::endl;

	testflag = false;
	PGresult *result = PQexec(dbh, cmd.c_str());

	if ((result != NULL) && (PQresultStatus(result) == PGRES_TUPLES_OK) &&
		PQntuples(result) > 0)
	{
		for (int pkgs = 0; pkgs < PQntuples(result); pkgs++)
		{
			testflag = true;
		}
	}

	PQclear(result);

	if (testflag == false)
	{
		return 0;
	}
	else
	{
		return 1;
	}
}

