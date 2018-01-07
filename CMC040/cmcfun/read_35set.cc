//! \file
//! \brief Read record from the set file
// %SBTTL "READ_35SET"
// %IDENT "V3.6a Calico"

//
// Source: ../../CMC030/cmcfun/source/read_35set.bas
// Translated from Basic to C++ using btran
// on Tuesday, November 28, 2017 at 16:13:33
//

#include <cstdlib>
#include <cstring>
#include <unistd.h>
#include "basicfun.h"

#include "preferences.h"
#include "cmcfun.h"
#include "database.h"

#include "utl/utl_set.h"

//!
//!	This function reads the SET file to get the device
//!	which the file exists on.  Will default device if it
//!	does not exist in the file.
//!
//! \author 09/26/84 - Kevin Handy
//!
long read_35set(
	std::string group,
		//!< The passed file name the user wants to get the device off
		//! of.
	std::string item,
		//!< The particular passed section of data on that file.
	utl_set_cdd &utl_set_read)
		//!< returned record
{
	long exit_status = CMC$_UNDEFINED;
	PGconn *dbh;		// Database connection
	PGresult *result;

	const char *cmd =
		"SELECT * FROM utl_set WHERE programname=$1 AND item=$2";

	dbh = db_conn.get_dbh();

	const char *param[3];
	param[0] = group.c_str();
	param[1] = item.c_str();

	result = PQexecParams(dbh,
		cmd,
		2,
		0,
		param,
		0,
		0,
		0);

	if ((result != NULL) && (PQresultStatus(result) == PGRES_TUPLES_OK) &&
		PQntuples(result) > 0)
	{
		for (int pkgs = 0; pkgs < PQntuples(result); pkgs++)
		{
			utl_set_read.load_psql(result, pkgs,
				utl_set_read.values);
			utl_set_read.copy_frommap(utl_set_read.values);

			exit_status = CMC$_NORMAL;
		}
	}

	PQclear(result);

	return exit_status;
}
