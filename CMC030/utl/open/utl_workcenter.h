/*
 * File Layout for: UTL.UTL_WORKCENTER on 21-May-01
 *
 * Work Center Profile
 */

#pragma member_alignment save
#pragma nomember_alignment

struct utl_workcenter_cdd
{
/* Element = LOCATION
   Description = Location number */
	char location[4];
/* Element = DEPT_NUM
   Description = Department number */
	char dept_num[6];
/* Element = WORK_CENTER
   Description = Work Center */
	char work_center[4];
/* Element =
   Description = Description */
	char description[40];
/* Element = PHONE
   Description = Phone number */
	char phone[10];
};
#pragma member_alignment restore
