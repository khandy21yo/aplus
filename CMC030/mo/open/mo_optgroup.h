/*
 * File Layout for: MO.MO_OPTGROUP on 21-May-01
 *
 * Option Group Description Table
 */

#pragma member_alignment save
#pragma nomember_alignment

struct mo_optgroup_cdd
{
/* Element =
   Description = Option Group Code */
	char optgroup[2];
/* Element =
   Description = Group Code Description */
	char descr[40];
/* Element =
   Description = Sequence Number */
	char sequence[4];
};
#pragma member_alignment restore
