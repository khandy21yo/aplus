/*	%TITLE "Returns the Current Date in YYYYMMDD Form"
 */
#pragma module date_today "V3.6 Calico"

/*
 *++
 *
 * Abstract:HELP
 *	Date>Today
 *	.b
 *	.lm +5
 *	Returns the current date in YYYYMMDD format (8 char).
 *	.lm -5
 *
 * Index:
 *
 * Parameters:
 *
 *	Returned value
 *		The current date in YYYYMMDD format (8 char)
 *
 * Example:
 *
 *	DATUM$ = DATE_TODAY
 *
 * AUTHOR:
 *
 *	03/20/86 - Kevin Handy
 *--
 */
/*
 * Include files
 */
#include <string>
#include <ctime>
#include <cstdlib>

/*
 * Main function
 */
std::string date_today(void)
{
	char outstr[200];
	time_t t;
	struct tm *tmp;

	t = time(NULL);
	tmp = localtime(&t);
	if (tmp == NULL)
	{
		perror("localtime");
		exit(EXIT_FAILURE);
	}

	if (strftime(outstr, sizeof(outstr), "%Y%m%d", tmp) == 0)
	{
		fprintf(stderr, "strftime returned 0");
		exit(EXIT_FAILURE);
	}

	return outstr;
}
