/*
 * File Layout for: SB.SB_CLASS on 21-May-01
 *
 * Subaccount Class Description
 */

#pragma member_alignment save
#pragma nomember_alignment

struct sb_class_cdd
{
/* Element = CLASS
   Description = Class */
	char class[4];
/* Element = DESCRIPTION5
   Description = Description */
	char descr[30];
};
#pragma member_alignment restore
