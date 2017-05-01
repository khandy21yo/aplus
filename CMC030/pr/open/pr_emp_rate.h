/*
 * File Layout for: PR.PR_EMP_RATE on 21-May-01
 *
 * Payroll Employee Rate File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct pr_emp_rate_cdd
{
/* Element =
   Description = Employee number */
	char empnum[10];
/* Element = OPERATION
   Description = Operation */
	char oper[8];
/* Element =
   Description = Effective date */
	char effdat[8];
/* Element =
   Description = Hourly,Salary,Piece,Mile */
	char rate_type[1];
/* Element =
   Description = Rate code */
	char rate_cde[2];
/* Element =
   Description = Hourly Rate */
	double hour_rate;
/* Element =
   Description = Piece Salary */
	double piece_rate;
/* Element =
   Description = Overtime Percentage */
	int factor;
/* Element =
   Description = Standard efficiency rating */
	double stdeff;
/* Element = DATE
   Description = Evaluation Date */
	char eval_date[8];
};
#pragma member_alignment restore
