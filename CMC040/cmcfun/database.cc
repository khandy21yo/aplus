//
// database.cc
//
// Implemet classes defined in database.h
//
#include <iostream>
#include "database.h"

//
// Global connection
//
db_connection db_conn;

//
// Connect to database
//
int db_connection::connect()
{
	std::string connstr =
		"dbname=" + db_username() +
	       " application_name=" __FILE__;
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
}

//
// Disconnect from database
//
int db_connection::disconnect()
{
	PQfinish(dbh);
}

