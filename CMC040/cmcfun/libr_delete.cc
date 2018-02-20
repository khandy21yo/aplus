//! \file
//! \file Delete a Key from a Library
// %SBTTL "LIBR_DELETE"
// %IDENT "V3.3"
//
// Source: ../../CMC030/cmcfun/source/libr_delete.bas_old
// Translated from Basic to C++ using btran
// on Monday, February 19, 2018 at 16:47:56
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
//!	This function will delete a key from a library,
//!	and will delete the text associated with the key
//!	if there are no other keys pointing to it.
//!
//! Parameters:
//!
//!	LIB_NAME$
//!		The passed library to remove key from.
//!
//!	KEY_NAME$
//!		The passed name of the key to delete.
//!
//!	Returns a status code.
//!
//! Example:
//!
//!	ST% = LIBR_DELETE("HELP_GL", "REMOVE_KEY")
//!
//! Author:
//!
//!	01/07/87 - Kevin Handy
//!
long libr_delete(
	const std::string &lib_name,
	const std::string &key_name)
{

	PGconn *dbh;		// Database connection
	int Result = 0;

	dbh = db_conn.get_dbh();

	//
	// Assume the library exists, because previous function
	// should have tested that already.
	//
	std::string cmd =
		"DELETE FROM " +
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

	if ((result != NULL) && (PQresultStatus(result) == PGRES_COMMAND_OK))
	{
		Result = 1;
	}
	else
	{
		Result = 0;
	}
	PQclear(result);
	return Result;
}
