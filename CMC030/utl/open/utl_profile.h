/*
 * File Layout for: UTL.UTL_PROFILE on 21-May-01
 *
 * Company Profile
 */

#pragma member_alignment save
#pragma nomember_alignment

struct utl_profile_cdd
{
/* Element =
   Description = Company name for menu */
	char menu_name[30];
/* Element =
   Description = Company name for report */
	char rep_name[30];
/* Element = LOCATION
   Description = Main Office Location number */
	char mainlocation[4];
/* Element = LOCATION
   Description = Default Location number */
	char deflocation[4];
};
#pragma member_alignment restore
