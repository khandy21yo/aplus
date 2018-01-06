//! \file cmcfun.h
//
//! \brief Simple functions in the cmcfun subdirectory.
//
//! This header defines simple functions in the cmcfun/ dubdirectory
//! that do not need special headers.
//!
//! Other headers contain more complex functions, such as scopedef.h
//
#ifndef _cmcfun_h_
#define _cmcfun_h_

#include <string>
#include <vector>

//
// Constants
//
//=================================================================
static const long CMC$_ABORT              =         10;
static const long CMC$_DATEOUT            =         11;
static const long CMC$_LEFT               =          0;
static const long CMC$_MACRO              =         19;
static const long CMC$_NOOPTION           =          4;
static const long CMC$_NORMAL             =          1;
static const long CMC$_RIGHT              =          1;
static const long CMC$_TERMINATED         =          8;
static const long CMC$_UNDEFINED          =          3;
static const long CMC$_UNTERROR           =          2;
static const long CMC$_WARNING            =          0;
//=================================================================
static const long OPT_ABORT               =          1;
static const long OPT_ADDREC              =          2;
static const long OPT_ASSIGN              =          3;
static const long OPT_CHECK               =          4;
static const long OPT_CLOSEFILE           =          5;
static const long OPT_COMPLETE            =          6;
static const long OPT_CONFIRM             =          7;
static const long OPT_HHMMSS              =          0;
static const long OPT_INTERRUPT           =          8;
static const long OPT_MARKFILE            =          9;
static const long OPT_MMDDYY              =          0;
static const long OPT_OPENFILE            =         10;
static const long OPT_POSTFILE            =         11;
static const long OPT_REPORT              =         14;
static const long OPT_RESTART             =         12;
static const long OPT_SUMMARY             =         13;
//=================================================================
static const long SUBOPT_CHECK            =      65536;
static const long SUBOPT_DETAIL           =     131072;
static const long SUBOPT_FINAL            =     262144;
static const long SUBOPT_LEDGER           =          1;
static const long SUBOPT_LINEITEM         =          2;
static const long SUBOPT_NOOPT            =          0;
static const long SUBOPT_REGISTER         =          3;
static const long SUBOPT_REVERSE          =     524288;

//
// Prototypes
//
std::string assg_makebatch(
	const std::string &given_date,
	const std::string &given_time);
void assg_unmakebatch(std::string &batch,
	std::string &given_date, std::string &given_time);
long comp_string(const std::string &test_string, 
	const std::string &wildcard_string);
long date_daycode(const std::string &day);
std::string date_invdcode(long daycode);
std::string date_today(void);
void find_file(
	const std::string &wildf,
	std::vector<std::string> &alist,
	int flag,
	const std::string &prefix,
	const std::string &suffix);
long find_fileexists(
	const std::string wildf,
	long flag);
double func_round(double xnum, int xprec);
long libr_3insert(const std::string &lib_name,
	const std::string &file_name,
	const std::string &key_name);
long libr_digsr(
	const std::string &lib_name,
	const std::string &key_name,
	std::vector<std::string> &code);
std::string prnt_date(
	const std::string &indate,
	long outlen);
std::string prnt_fancydate(
	const std::string &adate);
std::string read_sysjob(void);
std::string read_syspn(void);
long time_code(const std::string &datum);
std::string time_invcode(long tcode);
std::string time_now(void);
void writ_string(
	const std::string &Source,
	std::string &Result);

#endif

