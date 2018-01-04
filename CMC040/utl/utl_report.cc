//! \file
//! \brief implement utl_report.h class
//!
#include <string>

#include "basicfun.h"
#include "preferences"
#include "cmcfun.h"
#include "database.h"
#include "utl/utl_report.h"

//!
//! \brief Sesrch for a record, and retrieve data from datavase
//!
//! Tries to find a record in the rable.
//!
//! This is a 1st try at implementing the rms Get on an indexed file,
//! and shouldbevome part of the db_rmsindesed_cdd class. eventually.
//!
void utl_report::Get(
	const std::string &xrepnum);
	//!< Key tosarch for
{
	PGconn *dbh;		// Database connection
	PGresult *result;

	std::string cmd =
		"SELECT * FROM " +
		table_name +
		" WHERE repnum=$1 LIMIT 1";

	dbh = db_conn.get_dbh();

	const char *param[3];
	param[0] = xrepnum.c_str();

	result = PQexecParams(dbh,
		cmd.c_str(),
		1,
		0,
		param,
		0,
		0,
		0);

	if ((result != NULL) && (PQresultStatus(result) == PGRES_TUPLES_OK) &&
		PQntuples(result) > 0)
	{
		load_psql(result, 0, values);
		copy_frommap(values);
	}
	else
	{
		throw BasicError(155);
	}

}
