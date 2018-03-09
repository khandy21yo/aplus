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
		return 0;
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
		return 0;
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
//! \brief parameter stack
//!
//! This class holds a stack of parameters used to handle filling in
//! parameters in SQL statements (ie. $1, $2, ...)
//!
//! This will be used only for character values, numeric values will
//! be stuffed directly into the SQL command itself.
//!
//! This class exists to make it simpler to pass this information into
//! and out of functions that are building up the SQL commands.
//!
class db_params_cdd
{
public:
	int entries;
		//!< How many parameters have been defined
	const char *param[60];
		//!< Table of parameters.
		//!< Must point to null terminated char arrays.

public:
	//!
	//! \brief Constructor
	//!
	db_params_cdd(void)
	{
		entries = 0;
	}

	//!
	//! \brief Append a string to the list
	//!
	std::string append(const char *text)
	{
		param[entries++] = text;
		return '$' + std::to_string(entries);
	}
};

//!
//! \brief map to hold database values
//!
//! This hold infividual values in a key/data format.
//!
typedef std::map<std::string, std::string> db_map_cdd;

//!
//! \brief enum for allowed data types
//!
enum db_datatype_enum
{
	DB_TYPE_STRING =1,	//!< std::string
	DB_TYPE_LONG,		//!< long
	DB_TYPE_DOUBLE		//!< double (numeric)
};

//!
//! \brief Field definitions
//!
class db_field_cdd
{
public:
	int length;		//!< Maximum characters, digits, etc.
	int type;		//!< Typr of data in field

public:
	//!< Constructor
	db_field_cdd(
		int xlength = 0,	//!< Max length
		int xtype = 0)		//!< Data type
	{
		length = xlength;
		type = xtype;
	}
	//!
	//! \brief copy value from map into simple variable
	virtual void copy_frommap(
		const std::string &xvalue)	//!> Value to copy from
	{
	}
	//!
	//! \brief copy value from descrete value into map value
	//!
	virtual void copy_tomap(
		std::string &xvalue)		//!< Value to copy to
	{
	}
	//!
	//! \brief Dump values for debugging
	//!
	void dump(void)
	{
		std::cerr << "    Length: " << length <<
			", Type: " << type << std::endl;
	}
};

//!
//! \brief Field definitions
//!
class db_field_string_cdd : public db_field_cdd
{
public:
	std::string *valueptr;		//!< Pointer to descrete value

public:
	//!< Constructor
	db_field_string_cdd(
		std::string &xvalue,		//!< Pointer to string value
		int xlength = 0)		//!< Max string length
		: db_field_cdd(xlength, DB_TYPE_STRING)
	{
		valueptr = &xvalue;
	}

	//! \brief Copt value from map string into descrete variable
	virtual void copy_frommap(
		const std::string &xvalue)
	{
		*valueptr = xvalue;
	}
	//! \brief Copy value from deascrete variable into map string
	virtual void copy_tomape(
		std::string &xvalue)		//!< Value to assign
	{
		xvalue = *valueptr;
	}
};

//!
//! \brief list to define database fields
//!
//! This hold infividual values in a key/data format.
//!
typedef std::map<std::string, db_field_cdd> db_fieldmap_cdd;

//!
//! \brief base class for all rms type data files
//!
class db_rms_cdd
{
public:
	db_fieldmap_cdd fields;
		//!< List of field definitions
	db_map_cdd values;
		//!< std::map containing values read from a table
	std::string table_name;
		//!< Name of the database table

public:
	db_rms_cdd();
	//! \brief Assignment Operator. Only copys data, not definitions.
	db_rms_cdd & operator=(const db_rms_cdd &xdb)
	{
		values = xdb.values;
		copy_frommap(values);
		return *this;
	}
	~db_rms_cdd();

	int load_psql(PGresult *result, int row, db_map_cdd &dbmap);

	//!
	//! \brief Copy all fields from the values map to the
	//! descrete split out variables.
	//!
	void copy_frommap(db_map_cdd &dbmap)
	{
		for (auto loop = fields.begin();
			loop != fields.end();
			loop++)
		{
			(*loop).second.copy_frommap(values[(*loop).first]);
		}
	}
	//!
	//! \brief Copy all fields to the values map from the
	//! descrete split out variables.
	//!
	void copy_tomap(db_map_cdd &dbmap)
	{
		for (auto loop = fields.begin();
			loop != fields.end();
			loop++)
		{
			(*loop).second.copy_tomap(values[(*loop).first]);
		}
	}
	//!
	//! \brief Dump out informatopm in structure
	//!
	void dump(void)
	{
		std::cerr << "db_rms" << std::endl;
		std::cerr << "  Table: " << table_name << std::endl;
		for (auto loop = fields.begin();
			loop != fields.end();
			loop++)
		{
			std::cerr << "  Field: "<<
				(*loop).first <<
				std::endl;
			(*loop).second.dump();
		}
		for (auto loop1 = values.begin();
			loop1 != values.end();
			loop1++)
		{
			std::cerr << "  Value "<<
				(*loop1).first <<
				(*loop1).second <<
				std::endl;
		}
	}

	virtual long update(void);
//	virtual std::string update_where(db_params_cdd &params);
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

	//! \brief Assignment Operator
	db_rmsrelative_cdd & operator=(const db_rmsrelative_cdd &xdb)
	{
		db_rms_cdd::operator=(xdb);
		return *this;
	}
};

//!
//! \brief Base class for RMS indexed files
//!
class db_rmsindexed_cdd : public db_rms_cdd
{
public:
	long key;	//!< Which key is being refered to

public:
	//! \brief constructor
	db_rmsindexed_cdd()
	{
		key = 0;
	}
	//! \brief Assignment Operator
	db_rmsindexed_cdd & operator=(const db_rmsindexed_cdd &xdb)
	{
		db_rms_cdd::operator=(xdb);
		return *this;
	}
};



//
// There really should only be one of these for most cases
//
extern db_connection db_conn;


long read_period(
	const std::string &xoption,
	const std::string &xera,
	std::string &xperiod,
	std::string &xper_desc,
	std::string &xstatus,
	std::string &xstart_date,
	std::string &xfinish_date,
	long &xage);

#endif
