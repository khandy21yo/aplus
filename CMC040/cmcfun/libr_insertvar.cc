//! \file
//! \brief Insert a Text String into a Library
// %SBTTL "LIBR_INSERTVAR"
// %IDENT "V3.6a Calico"
//
// Source: ../../CMC030/cmcfun/source/libr_insertvar.bas
// Translated from Basic to C++ using btran
// on Friday, February 16, 2018 at 13:11:03
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
//!	This function will insert text into a library,
//!	and append a key to that text.
//!
//! Parameters:
//!
//!	LIB_NAME$
//!		Passed name of library to insert text into.
//!
//!	TEXT$
//!		Passed text to insert into the library.
//!
//!	KEY_NAME$
//!		Passed name of key to append to text.
//!
//!	Returns a status code.
//!
//! Example:
//!
//!	ST% = LIBR_INSERT("HELP_FUN", "Happyville Party Club", "EVENT")
//!
//! Author:
//!
//!	01/13/93 - Kevin Handy
//!
long libr_insertvar(
	const std::string &lib_name,
	const std::string &text,
	const std::string &key_name)
{
	long Result = 1;
	PGconn *dbh;		// Database connection
	bool itexists = false;

	dbh = db_conn.get_dbh();

	//
	// Assume the library exists, because previous function
	// should have tested that already.
	//
	std::string cmd =
		"SELECT value FROM " +
		lib_name + "  WHERE libkey = $1";

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
		itexists = true;
	}

	PQclear(result);

	if (itexists)
	{
		//
		// Record already exists, so update it
		//
		std::string cmd =
			"UPDATE " +
			lib_name + " SET value=$2 WHERE libkey = $1";

		params[0] = key_name.c_str();
		params[1] = text.c_str();

		PGresult *result = PQexecParams(dbh,
			cmd.c_str(),
			2,
			0,
			params,
			0,
			0,
			0);

		//! \todo Fix status check proprtly
		if (result == NULL)
		{
			Result = 0;
		}
	}
	else
	{
		//
		// Record doesn't exist, so ad it
		//
		std::string cmd =
			"INSERT INTO " +
			lib_name + "(libkey, value) VALUES ($1, $2)";

		params[0] = key_name.c_str();
		params[1] = text.c_str();

		PGresult *result = PQexecParams(dbh,
			cmd.c_str(),
			2,
			0,
			params,
			0,
			0,
			0);

		//! \todo Fix status check proprtly
		if (result == NULL)
		{
			Result = 0;
		}

	}

	return Result;
}

