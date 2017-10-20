/* %TITLE "Generate a batch number given a date and a time"
 */
#pragma module assg_makebatch "V3.6 Calico"

/*
 *++
 *
 * Abstract:HELP
 *	.b
 *	.lm +5
 *	This functon will generate a batch number when given
 *	a date and time.
 *	.b
 *	The method used to generate a batch number is to calculate
 *	the number of seconds since the beginning of the system's
 *	date, and then stuff as much of that into a RADIX(32?)
 *	value as possible. The loss of the high order bits causes
 *	the value to cycle every 68 years.
 *	.lm -5
 *
 * Index:
 *
 * Parameters:
 *
 *	GIVEN_DATE$
 *		This is the date to generate the batch number for,
 *		in YYYYMMDD format.
 *
 *	GIVEN_TIME$
 *		This is the time to generate the batch number for,
 *		in HHMMSS format.
 *--
 */

/*
 * Include files
 */
#include <string>
#include <cmath>

/*
 * External functions
 */
long date_daycode(const std::string &UseDate);
long time_code(const std::string &UseTime);

/*
 * local constants
 */
static const char character[] = "23456789ABCDEFGHJKLMNPQRSTUVWXYZ";

/*
 * Main function
 */
std::string assg_makebatch(
	const std::string &given_date,
	const std::string &given_time)
{
	int alpha;
	int loop;
	char prior_number[7];
	double seconds;
	int radix;

	/*
	 * Define the RADIX and the characters used to print that RADIX.
	 * Note that some characters are missing, because trying to
	 * identify them can be hard from a printout, i.e. (0 and O,
	 * 1 and I)
	 */
	radix = sizeof(character) - 1;

	/*
	 * Generate the number of seconds since the beginning of time
	 * (according to the date function).
	 */
	seconds = date_daycode(given_date) * 60.0 * 60.0 * 24.0 +
		time_code(given_time);

	/*
	 * Trim it down to the maximum allowed for RADIX(char%)
	 */
	seconds = seconds -
		floor(seconds / pow(radix, 6.0)) * pow(radix, 6.0);

	/*
	 * Convert the number to RADIX(char%)
	 *
	 */
	prior_number[6] = '\0';
	for (loop = 5; loop >= 0; loop--)
	{
		alpha = seconds - floor(seconds / radix) * radix;
		seconds = floor(seconds / radix);
		prior_number[loop] = character[alpha];
	}

	/*
	 * Return output string
	 */
	return prior_number;
}
