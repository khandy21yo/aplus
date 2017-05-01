/*
 * File Layout for: UTL.UTL_COUNTY on 21-May-01
 *
 * County, Etc.
 */

#pragma member_alignment save
#pragma nomember_alignment

struct utl_county_cdd
{
/* Element = COUNTRY
   Description = Country */
	char country[2];
/* Element = STATE
   Description = State */
	char state[2];
/* Element = COUNTY
   Description = County */
	char county[2];
/* Element =
   Description = Description */
	char descr[40];
};
#pragma member_alignment restore
