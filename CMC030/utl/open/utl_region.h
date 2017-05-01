/*
 * File Layout for: UTL.UTL_REGION on 21-May-01
 *
 * Region Description File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct utl_region_cdd
{
/* Element = COUNTRY
   Description = Country */
	char country[2];
/* Element = REGION
   Description = Region number */
	char region[2];
/* Element = DESCRIPTION
   Description = Description */
	char description[30];
};
#pragma member_alignment restore
