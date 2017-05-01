/*
 * File Layout for: UTL.UTL_SET on 21-May-01
 *
 * CMC Utility Set File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct utl_set_cdd
{
/* Element =
   Description = Program name */
	char programname[39];
/* Element =
   Description = Item number */
	char item[6];
/* Element =
   Description = System name */
	char system[2];
/* Element = YESNO
   Description = Yes or No Flag for Undefined Input */
	char allowund[1];
/* Element =
   Description = Unused */
	char unused[3];
/* Element =
   Description = Hard/Soft/Field default */
	char hard[1];
/* Element =
   Description = Data */
	char sdata[30];
/* Element =
   Description = Data Format */
	char fdata[30];
/* Element =
   Description = Unused Fiels */
	char unused2[4];
};
#pragma member_alignment restore
