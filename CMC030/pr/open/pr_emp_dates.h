/*
 * File Layout for: PR.PR_EMP_DATES on 21-May-01
 *
 * Payroll Employee Date History
 */

#pragma member_alignment save
#pragma nomember_alignment

struct pr_emp_dates_cdd
{
/* Element = EMPLOYEE
   Description = Employee number */
	char employee[10];
/* Element =
   Description = Date Code */
	char datecd[2];
/* Element = DATE
   Description = Beginning Date */
	char datebegin[8];
/* Element = DATE
   Description = Ending Date */
	char dateend[8];
/* Element =
   Description = Description */
	char descr[30];
};
#pragma member_alignment restore
