
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
long libr_3insert(const std::string &lib_name,
	const std::string &file_name,
	const std::string &key_name);
long time_code(const std::string &datum);
std::string date_invdcode(long dcode);
std::string read_sysjob(void);
std::string time_invcode(long tcode);
std::string time_now(void);

#endif
