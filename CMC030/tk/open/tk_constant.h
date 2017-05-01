/*
 * File Layout for: TK.TK_CONSTANT on 21-May-01
 *
 * CMC Constatnts
 */

#pragma member_alignment save
#pragma nomember_alignment

struct tk_constant_cdd
{
/* Element = CONSTNAME
   Description = Constant name */
	char constname[39];
/* Element = CLASS
   Description = Class */
	char class[4];
/* Element =
   Description = Constatnt value */
	long const;
};
#pragma member_alignment restore
