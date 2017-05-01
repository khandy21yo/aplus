/*
 * File Layout for: UTL.UTL_PACKFORM on 21-May-01
 *
 * Packiging Form Description
 */

#pragma member_alignment save
#pragma nomember_alignment

struct utl_packform_cdd
{
/* Element = PACKFORM
   Description = Pack form code */
	char code[3];
/* Element =
   Description = Description */
	char description[20];
};
#pragma member_alignment restore
