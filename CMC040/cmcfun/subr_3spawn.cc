//!
//!	%TITLE "subr_3spawn - Function to Spawn A New Job In Foreground"
//!/
#pragma module subr_3spawn "V3.6 Calico"

/*
 * Include files
 */
#include <iostream>
#include <cstdlib>
#include <string>

#include "preferences.h"
#include "cmcfun.h"
#include "scopedef.h"
#include "smg/smg.h"

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
	system(pname.c_str());
	update_panels();
}
