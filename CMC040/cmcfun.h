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
double func_round(double xnum, int xprec);
long libr_3insert(const std::string &lib_name,
	const std::string &file_name,
	const std::string &key_name);
std::string prnt_date(
	const std::string &indate,
	long outlen);
std::string read_sysjob(void);
std::string read_syspn(void);
long time_code(const std::string &datum);
std::string time_invcode(long tcode);
std::string time_now(void);

#endif
