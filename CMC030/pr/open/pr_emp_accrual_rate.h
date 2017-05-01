/*
 * File Layout for: PR.PR_EMP_ACCRUAL_RATE on 21-May-01
 *
 * Accrual Definition Rates
 */

#pragma member_alignment save
#pragma nomember_alignment

struct pr_emp_accrual_rate_cdd
{
/* Element = EMPLOYEE
   Description = Employee number */
	char empnum[10];
/* Element =
   Description = Accrual (Pay) Type */
	char atype[2];
/* Element = DATE
   Description = Start Date (YYYYMMDD) */
	char sdate[8];
/* Element =
   Description = Minimum hours worked to get */
	double minhour;
/* Element =
   Description = Maximum hours that get accrued for */
	double maxhour;
/* Element =
   Description = Rate per hour worked */
	double hourrate;
/* Element =
   Description = Maximum unused to allow */
	double maxaccrue;
/* Element =
   Description = Rate Code if we ever need it */
	char ratecode[1];
/* Element =
   Description = Periods to accrue for (12345S) */
	char freq[6];
};
#pragma member_alignment restore
