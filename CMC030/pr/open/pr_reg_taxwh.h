/*
 * File Layout for: PR.PR_REG_TAXWH on 21-May-01
 *
 * Payroll Tax Register
 */

#pragma member_alignment save
#pragma nomember_alignment

struct pr_reg_taxwh_cdd
{
/* Element =
   Description = Employee number */
	char empnum[10];
/* Element =
   Description = FW, FI, SW, SX, CW, DW, EX */
	char ttype[2];
/* Element =
   Description = Used if type = SW,SX,CW,DW,EX */
	char code[2];
/* Element =
   Description = Quarterly wages */
	double qtrwag[4];
/* Element =
   Description = Quarterly taxes witheld */
	double qtrtax[4];
/* Element =
   Description = Weeks worked during quarter */
	int wkwrk[4];
/* Element =
   Description = Batch number from update */
	int update_counter;
};
#pragma member_alignment restore
