/*
 * File Layout for: EL.EL_CLASS on 21-May-01
 *
 * Equipment Ledger Class Description File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct el_class_cdd
{
/* Element = CLASS
   Description = Class */
	char class[4];
/* Element = DESCRIPTION
   Description = Description */
	char descr[40];
};
#pragma member_alignment restore
