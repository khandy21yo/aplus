/*
 * File Layout for: PR.PR_EMP_ACCRUAL on 21-May-01
 *
 * Accrual Definition Header
 */

#pragma member_alignment save
#pragma nomember_alignment

struct pr_emp_accrual_cdd
{
/* Element = EMPLOYEE
   Description = Employee number */
	char empnum[10];
/* Element =
   Description = Accrual (Pay) Code */
	char atype[2];
/* Element =
   Description = Hours Unavailable */
	double hoursuna;
/* Element =
   Description = Hours Available */
	double hoursava;
/* Element =
   Description = Dollars Unavailable */
	double dollaruna;
/* Element =
   Description = Dollars Available */
	double dollarava;
/* Element =
   Description = When to make available */
	char availflag[1];
/* Element = DATE
   Description = Date (YYYYMMDD) just in case */
	char availdate[8];
/* Element = YESNO
   Description = Post Accrual to GL? */
	char glflag[1];
/* Element = BATCH
   Description = Batch number used for process (post,clos */
	char batch[6];
};
#pragma member_alignment restore
