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
		//!  returned record
{
	long Result;
	std::string a;
	std::string b;

	long exit_status;

// SELECT * FROM utl_set WHERE group=$1 AND item=$2

	//
	// Assume undefined
	//
	exit_status = CMC$_UNDEFINED;
	//
	// Try to read device name from DEVICE
	//
	a = std::string(utl_set.programname.size(), ' ');
	Lset(a, group);
	b = std::string(utl_set.item.size(), ' ');
	Lset(b, item);
	try
	{
		BasicChannel[utl_set_ch].SetKey(0);
		BasicChannel[utl_set_ch].SetKeyMode(Equal);
		BasicChannel[utl_set_ch].SetKeyValue(a + b);
		BasicChannel[utl_set_ch].SetRegardless();
		BasicChannel[utl_set_ch].Get();
	}
	catch(basic::BasicError &Be)
	{
		exit_status = CMC$_UNDEFINED;
		goto L_2000;
	}
	utl_set_read = utl_set;
	exit_status = CMC$_NORMAL;
L_2000:;
	return exit_status;
}
