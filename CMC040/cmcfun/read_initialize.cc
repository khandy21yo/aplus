//! \file
//! \brief Maintenance Initilization
// %SBTTL "READ_INITIALIZE"
// %IDENT "V3.6a Calico"
//
// Source: /home/kevin/aplus/CMC030/cmcfun/source/read_initialize.bas
// Translated from Basic to C++ using btran
// on Sunday, November 26, 2017 at 12:40:42
//

#include <cstdlib>
#include <string>
#include <cstring>
#include <unistd.h>
#include "basicfun.h"

#include "preferences.h"
#include "cmcfun.h"
#include "scopedef.h"
#include "database.h"
#include "utl/utl_profile.h"
#include "utl/utl_set.h"
#include "smg/smg.h"

//!
//! \brief Global definition of "scope".
//!
//! This should be the only true definition of the "scope"
//! cariable. All others should refer to this one.
//!
scope_struct scope;

//! \brief Maintenance Initilization
//!
//!	This subroutine initializes the maintenance programs.
//!
//!
//! Example:
//!
//!	CALL READ_INITIALIZE
//!
//! \author 02/23/87 - Kevin Handy
//
void read_initialize(void)
{
	long i;
	std::string junk;
	long smg_cols;
	long smg_rows;
	long smg_status;
	long utl_profile_ch;
	long v;

// #pragma psect static_rw utl_profile,gbl,ovr
	utl_profile_cdd utl_profile;
// #pragma psect end utl_profile
	utl_set_cdd utl_set_read;

	//
	// Get the program name
	//
	scope.prg_program = read_syspn();
	scope.prg_ident = "H";
	scope.prg_item = "HELP";
	//******************************************************************
	// Keyboard open routine
	//******************************************************************
	if (scope.screen_width < 80)
	{
		scope.screen_width = 80;
	}
	if (scope.screen_width > 80)
	{
		scope.screen_width = 132;
	}
	//
	// Create the pasteboard
	//
	smg_status = smg$create_pasteboard(scope.smg_pbid,
		0, 0, &smg_rows, &smg_cols);
	if ((smg_status & 1) == 0)
	{
		exit(smg_status);
	}
	//
	// Change width if necessary
	//
	if (scope.screen_width != smg_cols)
	{
		smg_status = smg$change_pbd_characteristics(scope.smg_pbid,
			scope.screen_width, &smg_cols, 0, &smg_rows);
	}
	//
	// Create the Message display
	//
	// 2 Rows
	// Columns
	// Identifier
	// No border
	// No attributes
	// Default character set
	smg_status = smg$create_virtual_display(2, 132,
		scope.smg_message, 0, 0, 0);
	if ((smg_status & 1) == 0)
	{
		exit(smg_status);
	}
	//
	// Paste the data display
	//
	// Message display
	// Pasteboard
	// Row to start in
	// Column to start in
	// Don't need top-disp
	smg_status = smg$paste_virtual_display(scope.smg_message,
		scope.smg_pbid, 23, 1, 0);
	if ((smg_status & 1) == 0)
	{
		exit(smg_status);
	}
	//
	// Create the option display
	//
	// 2 Rows
	// Columns
	// Identifier
	// No border
	// No attributes
	// Default character set
	smg_status = smg$create_virtual_display(2, 132,
		scope.smg_option, 0, 0, 0);
	if ((smg_status & 1) == 0)
	{
		exit(smg_status);
	}
	//
	// Paste the data display
	//
	// Option Display
	// Pasteboard
	// Row to start in
	// Column to start in
	// Don't need top-disp
	smg_status = smg$paste_virtual_display(scope.smg_option,
		scope.smg_pbid, 21, 1, 0);
	if ((smg_status & 1) == 0)
	{
		exit(smg_status);
	}
	//
	// Paste the data display
	//
	smg_status = smg$create_virtual_keyboard(scope.smg_kbid);
	if ((smg_status & 1) == 0)
	{
		exit(smg_status);
	}
	//
	// Remove the cursor
	//
	smg_status = smg$set_cursor_mode(scope.smg_pbid, 1);
	//
	// Set up hardware form feed so reports will page correctly
	// through printer port.
	//
	i = smg$set_term_characteristics(scope.smg_pbid,
		TT$M_MECHFORM, 0, TT$M_WRAP, 0);
	//
	// Load in company name
	//
	try
	{
		utl_profile.get_record(1);
	}
	catch(basic::BasicError &Be)
	{
		utl_profile.menu_name = "";
		utl_profile.rep_name = "";
		utl_profile.mainlocation = "";
		utl_profile.deflocation = "";
	}
	scope.prg_company = utl_profile.menu_name;
	//
	// Turn on timeout if desired
	//
	v = read_35set("TIME", "OUT", utl_set_read);
	junk = basic::edit(utl_set_read.sdata, 2 + 4);
	if (junk != "")
	{
		scope.scope_timeout = std::stol(junk);
	}
}
