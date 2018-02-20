//! \file
//! \brief Edit/Create File from Text Library
// %SBTTL "LIBR_EDIT"
// %IDENT "V3.6a Calico"
//
// Source: ../../CMC030/cmcfun/source/libr_edit.bas
// Translated from Basic to C++ using btran
// on Monday, February 19, 2018 at 22:37:41
//

#include <cstdlib>
#include <cstring>
#include <unistd.h>
#include "basicfun.h"

#include "preferences.h"
#include "cmcfun.h"
#include "scopedef.h"


extern scope_struct scope;


//!
//! Abstract:HELP
//!	.p
//!	This function will edit text stored in a text
//!	library.  It places user into the EDT editor,
//!	and allows the text to be changed.  It will
//!	change text for all keys connected to the same
//!	text.
//!
//! Parameters:
//!
//!	LIB_NAME$
//!		The passed name of the library containing
//!		the text to be edited.
//!
//!	KEY_NAME$
//!		The passed key that points to the text in the
//!	library.
//!
//!	Returns a status code.
//!
//! Example:
//!
//!	ST% = LIBR_EDIT("HELP_GL", "ADDRESS")
//!
//! Author:
//!
//!	07/06/87 - Kevin Handy
//!
long libr_edit(
	const std::string &lib_name,
	const std::string &key_name)
{
	long Result;
	long smg_status;
	long st;
	std::string temp;
	std::string temp_file;
	long v;

	//
	// Think up temp file name
	//
	temp_file = std::string("/tmp/temp") + read_sysjob() + ".tmp";
	//
	// Extract help from library
	//
	st = libr_extract(lib_name, temp_file, key_name);
	//
	// Call the editor EDT
	//
	help_34message(scope, "editing...", "I", "LIBR_EDIT", key_name, "EDIT");
	// Cursor On
	st = smg$set_cursor_mode(scope.smg_pbid, 0);
	endwin();

#if 0
	st = edt$edit(temp_file, 0, 0, 0, 0, 0, 0, 0);

	entr_3message(scope, "", 1);
	//
	// Normal exit from EDT
	//
	if (st & 1)
	{
		//
		// Insert it in the library
		//
		st = libr_3insert(lib_name, temp_file, key_name);
		if ((st & 1) == 0)
		{
			temp = std::string("Unable to add text to file ") + std::to_string(st);
			entr_3message(scope, temp, 0);
		}
	}
	//
	// Error using EDT
	//
	else
	{
		exit(smg_status);
	}
#else
	system(("vi " + temp_file).c_str());

	entr_3message(scope, "", 1);
		//
	// Insert it in the library
	//
	st = libr_3insert(lib_name, temp_file, key_name);
	if ((st & 1) == 0)
	{
		temp = std::string("Unable to add text to file ") + std::to_string(st);
		entr_3message(scope, temp, 0);
	}
#endif
	// Off
	smg_status = smg$set_cursor_mode(scope.smg_pbid, 1);
	//*******************************************************************
	// Clean exit from function
	//*******************************************************************
	//
	// Kill temp files
	//
	v = unlink(temp_file.c_str());
	return Result;
}
