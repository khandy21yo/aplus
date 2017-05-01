/*
 * File Layout for: UTL.UTL_ERA on 21-May-01
 *
 * Accounting Era Description
 */

#pragma member_alignment save
#pragma nomember_alignment

struct utl_era_cdd
{
/* Element = ERA
   Description = Era code */
	char era[2];
/* Element =
   Description = Era description */
	char description[20];
/* Element = DATE
   Description = Beginning date */
	char beg_date[8];
};
#pragma member_alignment restore
