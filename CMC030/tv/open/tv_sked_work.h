/*
 * File Layout for: TV.TV_SKED_WORK on 21-May-01
 *
 * File Used to Hold Commercials to Schedule
 */

#pragma member_alignment save
#pragma nomember_alignment

struct tv_sked_work_cdd
{
/* Element =
   Description = Priority */
	int priority;
/* Element = TV_FRMNUM
   Description = Form Number */
	char frmnum[8];
/* Element = TV_CUSNUM
   Description = Customer Number */
	char cusnum[10];
/* Element =
   Description = Schedule number */
	char sked_num[2];
};
#pragma member_alignment restore
