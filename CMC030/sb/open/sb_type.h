/*
 * File Layout for: SB.SB_TYPE on 21-May-01
 *
 * Subaccount Type Description
 */

#pragma member_alignment save
#pragma nomember_alignment

struct sb_type_cdd
{
/* Element =
   Description = Type */
	char ttype[2];
/* Element = DESCRIPTION5
   Description = Description */
	char descr[30];
};
#pragma member_alignment restore
