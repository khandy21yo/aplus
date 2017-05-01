/*
 * File Layout for: MO.MO_MAKECLASS on 21-May-01
 *
 * Make Class Description Table
 */

#pragma member_alignment save
#pragma nomember_alignment

struct mo_makeclass_cdd
{
/* Element =
   Description = Class Code */
	char class[4];
/* Element =
   Description = Class Description */
	char descr[40];
};
#pragma member_alignment restore
