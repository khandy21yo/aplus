//! \file
//! \brief Return the Job Number of the Current Image
//
#pragma module read_sysjob "V3.6 Calico"

/*
 * Include files
 */
#include <string>

#include <sys/types.h>
#include <unistd.h>

//!
//! \brief Return the Job Number of the Current Image
//!
//!	This function returns the number of the current image as a string.
//!
//! \rteurns This function returns the job name of the current image.
//!
//! Example:
//!
//!	JOB$ = READ_SYSJOB
//!--
std::string read_sysjob(void)
{
	std::string result = std::to_string(getpid());
	return result;
}

