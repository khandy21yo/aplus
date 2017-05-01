/*
 * File Layout for: TV.TV_CUSTOM_CONFLICT on 21-May-01
 *
 * Conflict Codes for a Customer
 */

#pragma member_alignment save
#pragma nomember_alignment

struct tv_custom_conflict_cdd
{
/* Element =
   Description = Customer number */
	char cusnum[10];
/* Element =
   Description = Conflict code */
	char conflict[8];
};
#pragma member_alignment restore
