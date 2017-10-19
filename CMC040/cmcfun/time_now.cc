/*	%TITLE "Returns the Current Time in HHMMSS(24-Hour) Form"
 */
#pragma module time_now "V3.6 Calico"

/*
 *++
 *
 * Abstract:HELP
 *	.p
 *	Returns the current time in the HHMMSS (24-hour) form.
 *
 * Parameters:
 *
 *	Returns the current time in the HHMMSS form.
 *
 * Example:
 *
 *	NOW$ = TIME_NOW
 *
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
std::string time_now(void)
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

	if (strftime(outstr, sizeof(outstr), "H%M%S", tmp) == 0)
	{
		fprintf(stderr, "strftime returned 0");
		exit(EXIT_FAILURE);
	}

	return outstr;
}
