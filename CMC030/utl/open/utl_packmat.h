/*
 * File Layout for: UTL.UTL_PACKMAT on 21-May-01
 *
 * Packiging Material Description
 */

#pragma member_alignment save
#pragma nomember_alignment

struct utl_packmat_cdd
{
/* Element = PACKMAT
   Description = Pack material code */
	char code[2];
/* Element =
   Description = Description */
	char description[20];
};
#pragma member_alignment restore
