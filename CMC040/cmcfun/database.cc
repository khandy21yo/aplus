//! \file
//! \brief database.cc
//!
//! Implemet classes defined in database.h
//!
//! These classes are meant to make dealing with RMS type interactions
//! using PosstgreSQL instead of RMS somewhat easier.
//! If this was dealing with new code, it would probably be done a lot
//! differently, but this is to ese the conversion problems.
//!
#include <iostream>

#include "cmcfun.h"
#include "database.h"

//
// Global connection
//
db_connection db_conn;

// ************************************************
// db_connection
// ************************************************
//
//! \brief Connect to database
//
int db_connection::connect()
{
	std::string connstr =
		"dbname=" + db_username() +
	       " application_name=" __FILE__;
//	std::cerr << "connect " << connstr << std::endl;

	//
	// Attach to the SLP SQL database
	//
	dbh = PQconnectdb(connstr.c_str());
	if (dbh == NULL)
	{
		std::string junk;
		std::cerr << "**** Connection to database failed!" << std::endl;
		std::cin >> junk;
		exit(EXIT_FAILURE);
	}

	if (PQstatus(dbh) == CONNECTION_BAD)
	{
		const char *msg = PQerrorMessage(dbh);
		if (msg == NULL)
		{
			msg = "unknown error";
		}
		std::cerr << "Badd conection " << msg << std::endl;
		exit(EXIT_FAILURE);
	}
	isconnected = true;
	return 0;
}

//
//! \brief Disconnect from database
//
int db_connection::disconnect()
{
	PQfinish(dbh);
	return 0;
}

// ************************************************
// db_rms_cdd
// ************************************************

db_rms_cdd::db_rms_cdd()
{
	return;
}

db_rms_cdd::~db_rms_cdd()
{
	return;
}

//!
//! \brief Copy specific row into dbmap
//!
//! Copies individual fields from the psql result set into
//! the std::map "dbmap", keying off the field names.
//!
int db_rms_cdd::load_psql(PGresult *result, int row, db_map_cdd &dbmap)
{
	dbmap.clear();

	for (int cols = 0; cols < PQnfields(result); cols++)
	{
		char *fname = PQfname(result, cols);
		char *fvalue = PQgetvalue(result, row, cols);
		dbmap[fname] = fvalue;
	}

	return 0;
}

long db_rms_cdd::update(void)
{
	std::string cmd;		// SQL command being built
	db_params_cdd params;		// Parameter stack
	bool needcomma = false;		// Do we need to insert commas

	//
	// Build SQL command to update database
	//!
	cmd = "UPDATE " +  table_name + "SET ";

	for (auto loop = values.begin(); loop != values.end(); loop++)
	{
/*
		if ((*loop).first.changed())
		{
			if (needcomma)
			{
				cmd += ", ";
			}
			cmd += (*loop).first.update_set(paarams)
		}
*/
	}

	//
	// If needcomma is still false, then nothing is being changed,
	// so an update is unnecessary.
	//
	if (needcomma == false)
	{
		return 1;
	}

	//
	// Now the WHERE clause
	//
	cmd += update_where(params);

	//
	// Execute the command
	//
	abort();

	//
	// Done
	//
	return 1;
}


std::string db_rms_cdd::update_where(db_params_cdd &params)
{
	abort();
}


// ************************************************
// db_rmsrelative_cdd
// ************************************************

//!
//! \brief Get a record by its record number.
//!
int db_rmsrelative_cdd::get_record(int record)
{
	PGconn *dbh;		// Database connection
	PGresult *result;

	std::string cmd =
		std::string("SELECT * FROM ") +
		table_name +
		" WHERE recoid=$1";
	std::string recoid = std::to_string(record);

	dbh = db_conn.get_dbh();

	const char *param[3];
	param[0] = recoid.c_str();

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
	};

	return 0;
}

// ************************************************
// db_indexed_cdd
// ************************************************

