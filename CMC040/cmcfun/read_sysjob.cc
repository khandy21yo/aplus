/*	%TITLE "Return the Job Number of the Current Image"
 */
#pragma module read_sysjob "V3.6 Calico"

/*
 *++
 *
 * Abstract:HELP
 *	.lm +5
 *	.b
 *	This function returns the name of the current image.
 *	.lm -5
 *
 * Parameters:
 *
 *	This function returns the job name of the current image.
 *
 * Example:
 *
 *	JOB$ = READ_SYSJOB
 *--
 */

/*
 * Include files
 */
#include <string>

#include <sys/types.h>
#include <unistd.h>

/*
 * Main function
 */
std::string read_sysjob(void)
{
	std::string result = std::to_string(getpid());
	return result;
}

