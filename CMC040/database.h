//! \file database.h
//!
//! \brief Configuration preferences
// 
//! This file defines database parameters and classes to
//! define the connections to a PostgreSQL database.
//

#ifndef _database_h_
#define _database_h_

#include <string>
#include <map>

extern "C"
{
#include <unistd.h>
#include <postgresql/libpq-fe.h>
}

//
// This first section is used to define the connection parameters
// used when connecting to the database.
//
// They are set up here to connect to the local host for a database
// named with the current users name.
//
// Change these parameters as necessary
//

//!
//! \brief Database host name
//!
//! Defaults to current host
//!
inline std::string db_hostname(void)
{
	return std::string("localhost");
#if 0
	char hn[HOST_NAME_MAX];
	if (gethostname(hn, sizeof(hn)) == 0)
	{
		return std::string(hn);
	}
	else
	{
		return std::string("localhost");
	}
#endif
}

//!
//! \brief User name to use fot database
//!
inline std::string db_username(void)
{
	return std::string(getlogin());
}

//
//! \brief Class to help deal with the database connection
//
//! PostgreSQL can be quite fast, but making connections can
//! really slow things down. We also don't want to stay constantly
//! connected because that can tie up the database server with
//! unnecessary connections, since the default limit is 100
//! connections.
//!
//! We don't want to connect/disconnect
//! for each transaction, so this class is used to handle the
//! database connection.
//
class db_connection
{
private:
	bool isconnected;	//!< Tracks weither the db is currently
				//! connected or not.
	PGconn *dbh;		//!< PostgreSQL connection

public:
	//
	//! \brief constructor
	//
	db_connection()
	{
		isconnected = false;
	}
	//
	//! \brief Destructor
	//
	~db_connection()
	{
		try_disconnect();
	}
	//
	//! \brief Make sure we are currently connected.
	//
	int try_connect()
	{
		if (!isconnected)
		{
			connect();
		}
	}
	//
	// Connect. Assumes that it is not currently connected.
	// Use try_connect instead if connection might already
	// be established.
	//
	int connect();
	//
	//! \brief If connected, disconnect.
	//
	int try_disconnect()
	{
		if (isconnected)
		{
			disconnect();
		}
		isconnected = false;
	}
	//
	// Disconnect. Assumes we are connected.
	// If it might not be connected, use try_disconnedt() instead.
	//
	int disconnect();

	//
	//! \brief PostgreSQL connection
	//
	PGconn *get_dbh()
	{
		try_connect();
		return dbh;
	}
};

//!
//! \brief map to hold database values
//!
//! This hold infividual values in a key/data format.
//!
typedef std::map<std::string, std::string> db_map_cdd;

//!
//! \brief base class for all rms type data files
//!
class db_rms_cdd
{
public:
	db_map_cdd db_values;
		//!< std::map containing values read from a table
	std::string table_name;
		//!< Name of the database table

	int load_psql(PGresult *result, int row, db_map_cdd &dbmap);

	virtual void copy_tomap(db_map_cdd &dbmap);
	virtual void copy_frommap(db_map_cdd &dbmap);
};

//!
//! \brief Base class for RMS relative files
//!
//! Relative files are implemented by an SQL table with a special
//! key 'recoid' which contains the record number.
//! 'recoid' starts at 1 and goes up. 0 is not used, as RMS didn't use it.
//!
class db_rmsrelative_cdd : public db_rms_cdd
{
public:
	int get_record(int record);
};

//!
//! \brief Base class for RMS indexed files
//!
class db_rmsindexed_cdd : public db_rms_cdd
{
};



//
// There really should only be one of these for most cases
//
extern db_connection db_conn;

#endif
