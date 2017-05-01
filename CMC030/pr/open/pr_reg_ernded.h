/*
 * File Layout for: PR.PR_REG_ERNDED on 21-May-01
 *
 * Payroll Earnings and Deduction Register
 */

#pragma member_alignment save
#pragma nomember_alignment

struct pr_reg_ernded_cdd
{
/* Element =
   Description = Employee number */
	char empnum[10];
/* Element =
   Description = Deduction, noncompensaTion, Memo */
	char etype[1];
/* Element =
   Description = Ernded code */
	char code[2];
/* Element =
   Description = Quarter ernded dollars */
	double qtr_doll[4];
/* Element =
   Description = Regular hours */
	double reg_hrs[4];
/* Element =
   Description = Premium hours */
	double pre_hrs[4];
/* Element =
   Description = Units */
	double units[4];
/* Element =
   Description = Update counter */
	int update_counter;
};
#pragma member_alignment restore
