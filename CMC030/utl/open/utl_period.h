/*
 * File Layout for: UTL.UTL_PERIOD on 21-May-01
 *
 * Period Timekeeper
 */

#pragma member_alignment save
#pragma nomember_alignment

struct utl_period_cdd
{
/* Element = ERA
   Description = Era code */
	char era[2];
/* Element = YEAR
   Description = Physical year (YYYY) */
	char year[4];
/* Element = CYCLE
   Description = Period in the physical year */
	char cycle[2];
/* Element =
   Description = Period description */
	char description[20];
/* Element =
   Description = Period status flag */
	char period_status[1];
/* Element = DATE
   Description = Beginning date (YYYYMMDD) */
	char beg_date[8];
/* Element = DATE
   Description = End date (YYYYMMDD) */
	char end_date[8];
/* Element =
   Description = Period sequential number */
	char age[4];
};
#pragma member_alignment restore
