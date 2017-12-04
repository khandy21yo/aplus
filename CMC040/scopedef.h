//! \file scopedef.h
//!
//! \brief Defines scope type terminal functions so that
//! they are easily changed.
//
//
// Source: scopedef.com
// Translated from Basic to C++ using btran
// on Sunday, November 26, 2017 at 12:04:46
//
#ifndef _scopedef_h_
#define _scopedef_h_

#include <string>

#include "smg/smg.h"

//
//! \brief Global information about the display
//
struct scope_struct
{
	//! \brief Exit status of entry routines
	long scope_exit;
	//! \brief Time for input timeout in seconds
	long scope_timeout;
	//! \brief Keyboard buffer name
	smg_keyboard_id smg_kbid;
	//! \brief Pastboard buffer name
	smg_pasteboard_id smg_pbid;
	//! \brief Option display
	smg_display_id smg_option;
	//! \brief Message display
	smg_display_id smg_message;
	//! \brief Macro command flag
	long macroflag;
	//! \brief Company name
	std::string prg_company;
	//! \brief Message type
	std::string prg_ident;
	//! \brief Program Name
	std::string prg_program;
	//! \brief Item for help
	std::string prg_item;
	//! \brief Width of created screen
	long screen_width;
	//! \brief Interupt Menu Levels
	long imenu_levels;
};

//!
//! \brief option lists
//!
class option_list_struct
{
public:
	unsigned char option;	//!< Character to type to select
	int optionptr;		//!< Pointer to option character in name
	std::string name;	//!< Entire word for option
	int curoppos;		//!< Position in original string
	int line;		//!< Line option will display on
	int column;		//!< Column will display on
};

//
// Prototypes
//
std::string entr_3option(
	scope_struct &scope,
	std::string &op_group,
	std::string &op_possible,
	long &op_curop,
	long op_flagw);
long entr_4entry(
	scope_struct &scope,
	smg_display_id &smg_option,
	long xflag);
long entr_4specialkeys(
	scope_struct &scope,
	smg_display_id &smg_option,
	long xflag,
	long retchar);
void help_34message(
	scope_struct &scope,
	const std::string &messages,
	const std::string &help_severity,
	const std::string &help_progname,
	const std::string &help_filename,
	const std::string &help_item);
void read_3broadcast(
	void *scope,
	void *a2,
	void *a3,
	void *a4);
void read_initialize(void);

#endif
