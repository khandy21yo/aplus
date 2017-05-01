/*
 * File Layout for: UTL.UTL_FOB on 21-May-01
 *
 * FOB Table
 */

#pragma member_alignment save
#pragma nomember_alignment

struct utl_fob_cdd
{
/* Element = CODE
   Description = FOB Code */
	char fobcode[2];
/* Element = DESCRIPTION
   Description = Description */
	char descr[40];
};
#pragma member_alignment restore
