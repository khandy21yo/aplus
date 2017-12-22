//!
//!	%TITLE "subr_3spawn - Function to Spawn A New Job In Foreground"
//!/
#pragma module subr_3spawn "V3.6 Calico"

/*
 * Include files
 */
#include <iostream>
#inlude <cstdlib>
#include <string>

#include "preferences.h"
#include "cmcfun.h"
#include "scopedef.h"
#include "smg/smg.h"

/*
 * Define sume necessary hard-coded text descriptor strings
 */
static $DESCRIPTOR(dsc_subname, "Subjob$ ");
static $DESCRIPTOR(dsc_mesg1, "SORRY! I am unable to spawn a new job!");
static $DESCRIPTOR(dsc_mesg2, "You may have too many spawned jobs.");
static $DESCRIPTOR(dsc_menupath, "CMC$MENUPATH");


//!
//!
//! Abstract:
//!
//!	Function to spawn a new process
//!
//! Parameters:
//!
//!	PNAME$ 
//!		Passed command/program to execute.
//!
//!	Returned value
//!		A value to spawn a new process.
//! 
//! Example:
//!
//!	subr_3spawn("TEMP.COM")
//!
//! Author:
//!
//!	Kevin Handy
//!
void subr_3spawn(
	scope_struct &scope,
	const std::string &pname)
{
	endwin();
	system(name.c_str();
	update_panels();
}
