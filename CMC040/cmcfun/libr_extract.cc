//! \file
//! \brief Extract a File from a Text Library
// %SBTTL "LIBR_EXTRACT"
// %IDENT "V3.3"
//
// Source: ../../CMC030/cmcfun/source/libr_extract.bas_old
// Translated from Basic to C++ using btran
// on Monday, February 19, 2018 at 21:11:43
//

#include <iostream>
#include <fstream>
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
//!	it into a specified text file.
//!
//! Parameters:
//!
//!	LIB_NAME$
//!		Passed name of the library to pull text from.
//!
//!	FILE_NAME$
//!		Passed name of file to write text into.
//!
//!	KEY_NAME$
//!		Passed key for text to be pulled.
//!
//!	Returns a status code.
//!
//! Example:
//!
//!	ST% = LIBR_EXTRACT("HELP_GL", "TEXT.FILE", "ADDRESS")
//!
//! Author:
//!
//!	07/01/87 - Kevin Handy
//!
long libr_extract(
	const std::string &lib_name,
	const std::string &file_name,
	const std::string &key_name)
{
	long Result = 0;
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
		//
		// We only want one result. There shouldn't be more.
		// Stuff it ino\to a file.
		//
		std::ofstream os(file_name);
		int pkgs = 0;
		os << PQgetvalue(result, 0, 0);
		os.close();
		Result = 1;
	}
	else
	{
		Result = 0;
	}

	PQclear(result);

	return Result;
}

