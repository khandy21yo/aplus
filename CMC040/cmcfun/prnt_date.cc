//! \file
//! \brief Format Date into MM/DD/YY or MM/DD/YYYY Format
//!/
#pragma module prnt_date "V3.6 Calico"

/*
 * Include files
 */
#include <cstdio>
#include <cstring>
#include <string>

#include "cmcfun.h"
#include "preferences.h"

//!
//! \brief Format Date into MM/DD/YY or MM/DD/YYYY Format
//!
//!	This function formats a date into MM/DD/YYYY format if
//!	INDATE% is 8% and into MM/DD/YY format otherwise.
//!
//! \returns This function formats a date into either MM/DD/YYYY or
//!	MM/DD/YY, depending on the users choice.
//!
//! Example:
//!
//!	PRNT_DATE(AP_OPEN::INVDAT,8%)
//!
//! \author 08/02/85 - Kevin Handy
//!
std::string prnt_date(
	const std::string &indate,
		//!< The passed date that is to be formatted.
	long outlen)
		//!< The passed integer that tells how the date should be
		//! formatted.
{
	int loop;
	long length;
	char worktext[12];
	char outtext[12];

	if ((indate.size() == 8) || (indate.size() == 0))
	{
		worktext[0] = '\0';
		length = 0;
	}
	else
	{
		strcpy(worktext, "20");
		length = 2;
	}
	strncat(worktext, indate.c_str(), indate.size());
	length += indate.size();

	/*
	 * Don't allow null's in input string
	 */
	for (loop = 0; loop < length; loop++)
	{
		if (worktext[loop] == '\0')
		{
			worktext[loop] = ' ';
		}
	}

	/*
	 * Fill string out to eight bytes with spaces
	 */
	for (loop = length; loop < 8; loop++)
	{
		worktext[loop] = ' ';
	}

	/*
	 * Generated formatted date
	 */
	if (outlen == 8)
	{
		sprintf(outtext, "%2.2s/%2.2s/%4.4s",
			worktext +4, worktext + 6, worktext);
	}
	else
	{
		sprintf(outtext, "%2.2s/%2.2s/%2.2s",
			worktext + 4, worktext + 6, worktext + 2);
	}

	/*
	 * Return string
	 */
	return outtext;
}
