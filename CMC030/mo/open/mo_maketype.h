/*
 * File Layout for: MO.MO_MAKETYPE on 21-May-01
 *
 * Make Type Description Table
 */

#pragma member_alignment save
#pragma nomember_alignment

struct mo_maketype_cdd
{
/* Element =
   Description = Type Code */
	char mtype[2];
/* Element =
   Description = Type Description */
	char descr[40];
};
#pragma member_alignment restore
