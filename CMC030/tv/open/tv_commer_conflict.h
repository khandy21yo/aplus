/*
 * File Layout for: TV.TV_COMMER_CONFLICT on 21-May-01
 *
 * TV Commercial Conflict Table
 */

#pragma member_alignment save
#pragma nomember_alignment

struct tv_commer_conflict_cdd
{
/* Element =
   Description = Conflict code */
	char code[8];
/* Element =
   Description = Major grouping code */
	char major_code[8];
/* Element =
   Description = Description */
	char descr[20];
};
#pragma member_alignment restore
