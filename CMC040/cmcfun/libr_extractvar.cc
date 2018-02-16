//! \file
//! \brief Extract a File from a Text Library
// %SBTTL "LIBR_EXTRACTVAR"
// %IDENT "V3.6a Calico"
//
// Source: ../../CMC030/cmcfun/source/libr_extractvar.bas
// Translated from Basic to C++ using btran
// on Friday, February 16, 2018 at 12:51:34
//

#include <cstdlib>
#include <cstring>
#include <unistd.h>
#include "basicfun.h"

#include "preferences.h"
#include "cmcfun.h"
#include "database.h"
#include "smg/lbr.h"


//!
//! Abstract:HELP
//!	.p
//!	This function pulls text out of a library, and puts
//!	it into a specified text string.  All text is run
//!	together into one string.
//!
//! Parameter:
//!
//!	LIB_NAME$
//!		Passed name of the library to pull text from.
//!
//!	OUT_TEXT$
//!		Outgoing text.
//!		Contains '13'C where a line break is required.
//!
//!	KEY_NAME$
//!		Passed key for text to be pulled.
//!
//!
//!	Returns a status code.
//!
//! Example:
//!
//!	ST% = LIBR_EXTRACTVAR("HELP_GL", TEXT$, "ADDRESS$NOTE")
//!
//! Author:
//!
//!	07/28/87 - Kevin Handy
//!
long libr_extractvar(
	const std::string &lib_name,
	std::string &out_text,
	const std::string &key_name)
{
	PGconn *dbh;		// Database connection

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
		for (int pkgs = 0; pkgs < PQntuples(result); pkgs++)
		{
			out_text = PQgetvalue(result, 0, 0);
		}
	}
	else
	{
		std::cerr << "\n\nlbr_lookup_key " <<
			PQresultErrorMessage(result) <<
			std::endl;
	}

	PQclear(result);

	return 1;
}
