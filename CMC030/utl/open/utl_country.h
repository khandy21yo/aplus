/*
 * File Layout for: UTL.UTL_COUNTRY on 21-May-01
 *
 * Country Definitions
 */

#pragma member_alignment save
#pragma nomember_alignment

struct utl_country_cdd
{
/* Element = COUNTRY
   Description = Country */
	char country[2];
/* Element =
   Description = Description */
	char descr[40];
};
#pragma member_alignment restore
