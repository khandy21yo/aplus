//
// Configuration preferences
// 
// This file defines database parameters and classes to
// define the connections to a PostgreSQL database.
//

#ifndef _database_h_
#define _database_h_

#include <string>

extern "C"
{
#include <unistd.h>
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

//
// Database host name
//
// Defaults to current host
//
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

//
// User name to use fot database
//
inline std::string db_username(void)
{
	return std::string(getlogin());
}

//
// Class to help deal with the database connection
//
// PostgreSQL can be quite fast, but making connections can
// really slow things down. We also don't want to stay constantly
// connected because that can tie up the database server with
// unnecessary connections, since the default limit is 100
// connections.
//
// We don't want to connect/disconnect
// for each transaction, so this class is used to handle the
// database connection.
//
class db_connection
{
private:
	bool isconnected;	// Tracks weither the db is currently
				// connected or not.

public:
	//
	// constructor
	//
	db_connection()
	{
		isconnected = false;
	}
	//
	// Destructor
	//
	~db_connection()
	{
		try_disconnect();
	}
	//
	// Make sure we are currently connected.
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
	// If connected, disconnect.
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
};

#endif
