/*
 * File Layout for: PR.PR_REG_TAXES on 21-May-01
 *
 * Payroll Tax Register
 */

#pragma member_alignment save
#pragma nomember_alignment

struct pr_reg_taxes_cdd
{
/* Element = EMPLOYEE
   Description = Employee number */
	char empnum[10];
/* Element =
   Description = FW,FI,SW,SX,SU,DW,EW,etc. */
	char ttype[2];
/* Element =
   Description = Tax code (Used if type=SW,SX,CW,DW,EX) */
	char code[2];
/* Element =
   Description = Quarterly Taxes Witheld */
	double tax[4];
/* Element =
   Description = Quarterly Reportable Wages */
	double reportable[4];
/* Element =
   Description = Quarterly Taxable Wages */
	double taxable[4];
/* Element =
   Description = Weeks Worked during Quarter */
	int wkwrk[4];
/* Element = UPCOUNT
   Description = Update counter */
	int update_counter;
};
#pragma member_alignment restore
