/*
 * File Layout for: UTL.UTL_MACROS on 21-May-01
 *
 * User Macros Definition
 */

#pragma member_alignment save
#pragma nomember_alignment

struct utl_macros_cdd
{
/* Element =
   Description = User Macro Command */
	char command[20];
/* Element = DESCRIPTION
   Description = Description */
	char description[40];
/* Element =
   Description = Menu Path or Command */
	char cmd[40];
};
#pragma member_alignment restore
