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

//!
//! \brief Global information about the display
//!
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
std::string date_3select(
	scope_struct &scope,
	const std::string &starting_date);
void dspl_splitcursor(
	const std::string &curstr,
	long &rxxx,
	long &cyyy);
void entr_3badkey(
	struct scope_struct &scope,
	long bad_value);
long entr_3choice(
	scope_struct &scope,
	const std::string &wposition,
	const std::string &wsize,
	std::vector<std::string> &text,
	std::string &selected,
	long flag,
	const std::string &title,
	const std::string &draw_cols,
	long border_pos);
std::string entr_3date(
	scope_struct &scope,
	smg_display_id &xx_vdid,
	const std::string &opt_cpos,
	const std::string &opt_prompt,
	std::string &opt_xdflt,
	long opt_flag,
	const std::string &opt_xformat,
	std::string &opt_deflt,
	long length);
long entr_3enter(
	struct scope_struct &scope,
	smg_display_id &xx_vdid,
	long cposy,
	long cposx,
	std::string &xstr,
	long &start,
	long flag);
long entr_3entrystring(
	scope_struct &scope,
	smg_display_id &smg_option,
	long xflag,
	long xlen,
	std::string &retstring);
void entr_3message(
	scope_struct &scope,
	const std::string &mesg,
	long int flag);
void entr_3messagenewwindow(
	scope_struct &scope, 
	const std::string &mesg, 
	long int flag);
double entr_3number(
	scope_struct &scope,
	smg_display_id &xx_vdid,
	const std::string &op_cpos,
	const std::string &op_prompt,
	double xdeflt,
	long op_flag,
	const std::string &op_xformat,
	std::string &op_deflt);
std::string entr_3option(
	scope_struct &scope,
	const std::string &op_group,
	std::string &op_possible,
	long &op_curop,
	long op_flagw);
std::string entr_3string(
	scope_struct &scope,
	smg_display_id &xx_vdid,
	const std::string &op_cpos,
	const std::string &op_prompt,
	const std::string &op_xstart,
	long op_flag,
	const std::string &op_xformat,
	std::string &op_deflt,
	long length);
std::string entr_3stringlist(
	scope_struct &scope,
	smg_display_id &xx_vdid,
	const std::string &op_cpos,
	const std::string &op_prompt,
	const std::string &op_xstart,
	long &op_flag,
	const std::string &op_xformat,
	std::string &op_xdeflt,
	std::vector<std::string> &op_vtext,
	const std::string &op_thetitle,
	const std::string &op_drawcols);
std::string entr_3time(
	scope_struct &scope,
	smg_display_id &xx_vdid,
	const std::string &opt_cpos,
	const std::string &opt_prompt,
	const std::string &opt_xdflt,
	long opt_flag,
	const std::string &opt_kind,
	std::string &opt_deflt);
std::string entr_3yesno(
	scope_struct &scope,
	smg_display_id &xx_vdid,
	const std::string &op_cpos,
	const std::string &op_prompt,
	const std::string &op_deflt,
	long op_flag,
	const std::string &op_xformat,
	std::string &op_xdeflt);
long entr_4entry(
	scope_struct &scope,
	smg_display_id &smg_option,
	long xflag);
long entr_4specialkeys(
	scope_struct &scope,
	smg_display_id &smg_option,
	long xflag,
	long retchar);
long entr_macro(
	scope_struct &scope);
std::string entr_period(
	smg_display_id &xx_vdid,
	const std::string &cpos,
	const std::string &prompt,
	const std::string &xdflt,
	long flag,
	const std::string &xformat,
	std::string &deflt);
int func_4scoseq(
	int x);
void help_3message(
	scope_struct &scope,
	const std::string &messages,
	const std::string &help_severity,
	const std::string &help_progname,
	const std::string &help_item);
void help_34message(
	scope_struct &scope,
	const std::string &messages,
	const std::string &help_severity,
	const std::string &help_progname,
	const std::string &help_filename,
	const std::string &help_item);
std::string libr_select(
	const std::string &lib_name,
	const std::string &lib_title,
	const std::string &lib_help,
	std::string &optlist);
void menu_3interrupt(
	scope_struct &scope);
void read_3broadcast(
	void *scope,
	void *a2,
	void *a3,
	void *a4);
void read_initialize(void);
void subr_3exitprogram(
	scope_struct &scope,
	const std::string &opt_docommand,
	const std::string &opt_corecommon);
void subr_3spawn(
        scope_struct &scope,
        const std::string &pname);

#endif
