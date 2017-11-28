//! \file
//! \brief Insert a Text File into a Library
//!
#pragma module libr_3insert "V3.6 Calico"

/*
 * Include files
 */
#include <iostream>
#include <string>
#include <fstream>

#include "preferences.h"
#include "database.h"
#include "cmcfun.h"

//!
//! \brief Insert a Text File into a Library
//!
//!	This function will insert text into a library,
//!	and append a key to that text.
//!
//!	WARNING: This version tries to suplicate the capabilities
//!	of the VMS lib$ functions, and isn't what a native SQL
//!	version should look like.
//!
//! \ returns Returns a status code.
//!
//! Example:
//!
//!	ST% = LIBR_3INSERT("HELP_GL", "SOURCE.FILE", "ADDRESS")
//!
long libr_3insert(
	const std::string &lib_name,
		//!< Passed name of library to insert text into.
	const std::string &file_name,
		//!> Passed name of file containing text to insert
		//! into the library.
	const std::string &key_name)
		//!< Passed name of key to append to text.
{
	int st = 0;		// Return status
	bool testflag;		// status flag
	PGconn *dbh;		// Database connection

	//
	// Just dump out a message for the moment
	//
//	std::cerr << "libr_3insert: Lib=" << lib_name <<
//		", source=" << file_name <<
//		", key=" << key_name <<
//		std::endl;

	dbh = db_conn.get_dbh();

	//
	// Does library already exist?
	//
	std::string cmd =
		"SELECT * FROM information_schema.tables WHERE table_name = '" +
		lib_name +
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

	//
	// If the library table doesn't exist, create it
	//
	if (testflag == false)
	{
//		std::cerr << "We need to create " << lib_name << std::endl;

		cmd = "CREATE TABLE " + lib_name +
			" (libkey TEXT PRIMARY KEY, value TEXT)";

		PGresult *result = PQexec(dbh, cmd.c_str());

		if ((result == NULL) || 
			(PQresultStatus(result) != PGRES_COMMAND_OK))
		{
			std::cerr << "Unable to create library!" <<
				PQresultErrorMessage(result) <<
				std::endl;
			PQclear(result);
			return -1;
		}

		PQclear(result);
	}
	//
	// If it does exist, delete any e4xisting row
	//
	else
	{
//		std::cerr << "We need to delete " << 
//			lib_name << " - " << key_name <<
//			std::endl;

		//
		// This has SQL injection possibilities
		//
		cmd = "DELETE FROM " + 
			lib_name +
			" WHERE libkey = '" +
			key_name +
			"'";

		PGresult *result = PQexec(dbh, cmd.c_str());

		if ((result == NULL) || 
			(PQresultStatus(result) != PGRES_COMMAND_OK))
		{
			std::cerr << "Unable to delete from library!" <<
				PQresultErrorMessage(result) <<
				std::endl;
			PQclear(result);
			return -1;
		}

		PQclear(result);
	}

	//
	// Yank text out of file
	//
	std::string fulltext;
	std::string oneline;

	std::ifstream inf(file_name);
	while (getline(inf, oneline))
	{
		fulltext += oneline;
		fulltext += "\n";
	}
	inf.close();

	//
	// Insrt\\ert text into table
	//
	cmd = "INSERT INTO " +
		lib_name +
		" (libkey, value) VALUES ($1,$2)";
	const char *params[3];

	params[0] = key_name.c_str();
	params[1] = fulltext.c_str();

	result = PQexecParams(dbh,
		cmd.c_str(),
		2,
		0,
		params,
		0,
		0,
		0);

	if ((result == NULL) || 
		(PQresultStatus(result) != PGRES_COMMAND_OK))
	{
		std::cerr << "Unable to delete from library!" <<
			PQresultErrorMessage(result) <<
			std::endl;

		PQclear(result);
		return -1;
	}
	PQclear(result);

	/*
	 * Return status
	 */
	return(st);
}
