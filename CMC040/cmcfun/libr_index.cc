//! \file
//! \brieg Get a list of contents of a text library
// %SBTTL "LIBR_INDEX"
// %IDENT "V3.3"
//
// Source: ../../CMC030/cmcfun/source/libr_index.bas_old
// Translated from Basic to C++ using btran
// on Tuesday, February 20, 2018 at 16:51:40
//

#include <cstdlib>
#include <cstring>
#include <unistd.h>
#include "basicfun.h"

#include "preferences.h"
#include "cmcfun.h"
#include "database.h"





//!
//! Abstract:HELP
//!	.p
//!	This function will generate an index of what modules
//!	are in a library matching a key.
//!
//! Index:
//!
//! Parameter:
//!
//!	LIB_NAME$
//!		Passed name of library to insert text into.
//!
//!	KEY_NAME$
//!		Passed name of key to append to text.
//!
//!	LIBR_INDEX$()
//!		List of modules matching the key, with
//!		subscript (0) set to the number of items therin.
//!
//!	Returns a status code.
//!
//! Example:
//!
//!	ST% = LIBR_INDEX("HELP_GL", "*", A$())
//!
//! Author:
//!
//!	01/07/87 - Kevin Handy
//!
//! I Dropped the unudex array value as the last parameter.
//
long libr_index(
	const std::string &lib_name,
	const std::string &key_name,
	std::vector<std::string> slibr_index)
{
	long Result = 0;
	PGconn *dbh;		// Database connection

	dbh = db_conn.get_dbh();

	//
	// Assume the library exists, because previous function
	// should have tested that already.
	//
	std::string cmd =
		"SELECT libket FROM " +
		lib_name + "  WHERE libkey ~ $1";

	const char *params[2];

	params[0] = key_name.c_str();

	PGresult *result = PQexecParams(dbh,
		cmd.c_str(),
		1,
		0,
		params,
		0,
		0,
		0);

	if ((result != NULL) && (PQresultStatus(result) == PGRES_TUPLES_OK) &&
		PQntuples(result) > 0)
	{
		slibr_index[0] = std::to_string(PQntuples(result));

		for (int pkgs = 0; pkgs < PQntuples(result); pkgs++)
		{
			slibr_index[pkgs + 1] = PQgetvalue(result, 0, 0);
		}
	}
	else
	{
		slibr_index[0] = "0";
		Result = 0;
	}

	PQclear(result);

	return Result;
}
