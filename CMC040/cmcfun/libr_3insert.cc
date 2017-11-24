/*	%TITLE "Insert a Text File into a Library"
 */
#pragma module libr_3insert "V3.6 Calico"

/*
 *
 *++
 *
 * Abstract:HELP
 *	.p
 *	This function will insert text into a library,
 *	and append a key to that text.
 *
 *	WARNING: This version should only be used when there is
 *	no chance of connected items.
 *
 * Parameter:
 *
 *	LIB_NAME$
 *		Passed name of library to insert text into.
 *
 *	FILE_NAME$
 *		Passed name of file containing text to insert
 *		into the library.
 *
 *	KEY_NAME$
 *		Passed name of key to append to text.
 *
 *	Returns a status code.
 *
 * Example:
 *
 *	ST% = LIBR_3INSERT("HELP_GL", "SOURCE.FILE", "ADDRESS")
 *
 *--
 */

/*
 * Include files
 */
#include <iostream>
#include <string>
#include "cmcfun.h"

#include "preferences.h"
#include "database.h"

/*
 * Main function
 */
long libr_3insert(const std::string &lib_name,
	const std::string &file_name,
	const std::string &key_name)
{
	int st = 0;		// Return status
	bool testflag;		// status flag
	PGconn *dbh;		// Database connection

	//
	// Just dump out a message for the moment
	//
	std::cerr << "libr_3insert: Lib=" << lib_name <<
		", source=" << file_name <<
		", key=" << key_name <<
		std::endl;

	dbh = db_conn.get_dbh();

	//
	// Does library already exist?
	//
	std::string cmd =
		std::string("SELECT EXISTS(SELECT * FROM information_schema.tables WHERE table_schema = '") +
		db_username() +
		"' AND table_name = '" +
		lib_name +
		"'";

	std::cerr << "Check for lib " << cmd << std::endl;

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

	//
	// If the library table doesn't exist, create it
	//
	if (testflag == false)
	{
		std::cerr << "We need to create " << lib_name << std::endl;

		cmd = "CREATE TABLE " + lib_name +
			"(libkey TEXT PRIMARY KEY, value TEXT)";

		PGresult *result = PQexec(dbh, cmd.c_str());

		if ((result == NULL) || 
			(PQresultStatus(result) == PGRES_COMMAND_OK))
		{
			std::cerr << "Unable to create library!" << std::endl;

			return -1;
		}
	}

	//
	// Yank text out of file
	//

	/*
	 * Return status
	 */
	return(st);
}
