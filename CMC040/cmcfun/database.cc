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

//
// Local specia working area for building SQL query
//
static const char *param[80];
	//!< Working area for building SQL parameters.
static int pcount;
	//!< Count of param[] filled in so far


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
// db_field_cdd
// ************************************************

//!
//! \brief Build the SET part of an SQL command for one field
//!
//! \returns the string to be appended to the SQL cmmand to update
//! this one field.
//!
std::string db_field_cdd::update_build_set(const std::string &name)
{
	return "";
}

// ************************************************
// db_field_string_cdd
// ************************************************

//!
//! \brief Build the SET part of an SQL command for one field
//!
//! \returns the string to be appended to the SQL cmmand to update
//! this one field.
//!
std::string db_field_string_cdd::update_build_set(const std::string &name)
{
	param[pcount++] = valueptr->c_str();
	std::string command = name + "=$" + std::to_string(pcount);
	return command;
}

// ************************************************
// db_rms_cdd
// ************************************************

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

//!
//! \brief Create the 'SET part of a SQL update string
//!
//! \returns the 'SET' part of a SQL statement
//!
std::string db_rms_cdd::update_build_set(const db_map_cdd &dbmap)
{
	bool needand = false;
	std::string buildstring;

	pcount = 0;

	//
	// Check all fields read from table
	//
	for (auto loop = dbmap.begin(); loop != dbmap.end(); loop++)
	{
		const std::string &key = (*loop).first;
		if (fields.count(key) != 0 &&
			fields[key].compare((*loop).second) == false)
		{
			if (needand == true)
			{
				buildstring += ", ";
			}
			needand = true;
			buildstring += fields[key].update_build_set(key);
		}
	}

	if (buildstring != "")
	{
		buildstring = "SET " + buildstring;
	}

	return buildstring;
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

