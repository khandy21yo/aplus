/*
 * File Layout for: UTL.UTL_DEPARTMENT on 21-May-01
 *
 * Department Profile
 */

#pragma member_alignment save
#pragma nomember_alignment

struct utl_department_cdd
{
/* Element = LOCATION
   Description = Location number */
	char location[4];
/* Element = DEPT_NUM
   Description = Department number */
	char dept_num[6];
/* Element =
   Description = Department name */
	char description[40];
/* Element = DEPGROUP
   Description = Department group number */
	char depgroup[2];
/* Element = PHONE
   Description = Phone number */
	char phone[10];
/* Element =
   Description = Department Supervisor */
	char supervisor[30];
};
#pragma member_alignment restore
