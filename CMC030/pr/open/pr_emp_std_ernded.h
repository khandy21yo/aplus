/*
 * File Layout for: PR.PR_EMP_STD_ERNDED on 21-May-01
 *
 * Payroll Employee Standard ERNDED File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct pr_emp_std_ernded_cdd
{
/* Element =
   Description = Employee Number */
	char empnum[10];
/* Element =
   Description = Payment,Deduction,noncompensaTion,Memo */
	char rtype[1];
/* Element =
   Description = Earnings/Deduction code */
	char code[2];
/* Element =
   Description = Rate or amount of Earn/Ded */
	double rate;
/* Element =
   Description = Limit to pay or deduction */
	double limit;
/* Element =
   Description = Amount earn/ded to date */
	double ctdbal;
/* Element =
   Description = Amount accrued */
	double accrued;
/* Element =
   Description = Date to stop paying/ded */
	char enddat[8];
/* Element =
   Description = Earn/Ded frequency */
	char freq[6];
/* Element =
   Description = 1-hourly,2-mile,3-gross,4-net,5-per pay */
	char method[1];
/* Element =
   Description = User defined */
	char userdef[30];
};
#pragma member_alignment restore
