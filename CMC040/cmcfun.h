
#ifndef _cmcfun_h_
#define _cmcfun_h_

std::string assg_makebatch(
	const std::string &given_date,
	const std::string &given_time);
void assg_unmakebatch(std::string &batch,
	std::string &given_date, std::string &given_time);
std::string date_today(void);
long date_daycode(const std::string &day);
long time_code(const std::string &datum);
std::string date_invdcode(long dcode);
std::string time_invcode(long tcode);
std::string time_now(void);

#endif
