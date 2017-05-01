/*
 * File Layout for: MO.MO_MODELCODE on 21-May-01
 *
 * Model Description Table
 */

#pragma member_alignment save
#pragma nomember_alignment

struct mo_modelcode_cdd
{
/* Element =
   Description = Model Code */
	char modelcode[4];
/* Element =
   Description = Model Description */
	char descr[40];
};
#pragma member_alignment restore
