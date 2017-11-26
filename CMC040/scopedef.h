// SCOPEDEF.COM - Defines scope type terminal functions so that
// they are easily changed.
//
//
// Source: scopedef.com
// Translated from Basic to C++ using btran
// on Sunday, November 26, 2017 at 12:04:46
//
#ifndef _scopedef_h_
#define _scopedef_h_

#include <string>

//
// Common area
//
// %NOCROSS
struct scope_struct
{
	// Exit status of entry routines
	long scope_exit;
	// Time for input timeout in seconds
	long scope_timeout;
	// Keyboard buffer name
	long smg_kbid;
	// Pastboard buffer name
	long smg_pbid;
	// Option display
	long smg_option;
	// Message display
	long smg_message;
	// Macro command flag
	long macroflag;
	// Company name
	std::string prg_company;
	// Message type
	std::string prg_ident;
	// Program Name
	std::string prg_program;
	// Item for help
	std::string prg_item;
	// Width of created screen
	long screen_width;
	// Interupt Menu Levels
	long imenu_levels;
};

//
// Prototypes
//
void read_initialize(void);

#endif
