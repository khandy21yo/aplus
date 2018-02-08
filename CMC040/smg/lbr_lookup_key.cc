//! \file
//! \brief Lookup key in library
//!

#include <iostream>

#include "database.h"
#include "smg/lbr.h"

//!
//! \brief Lookup key
//!
long lbr$lookup_key(
	lbr_index_cdd &lr_index,
	const std::string &key_name,
	long txrfa)
{
	PGconn *dbh;		// Database connection

	dbh = db_conn.get_dbh();

	//
	// Assume the library exists, because previous function
	// should have tested that already.
	//
	std::string cmd =
		"SELECT value FROM " +
		lr_index.name + "  WHERE libkey = $1";

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
			lr_index.datum = PQgetvalue(result, 0, 0);
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

