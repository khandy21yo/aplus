/*
 * File Layout for: PR.PR_TAX_PROFILE_S on 21-May-01
 *
 * Payroll Tax Profile - State
 */

#pragma member_alignment save
#pragma nomember_alignment

struct pr_tax_profile_s_cdd
{
/* Element =
   Description = 'S' for state */
	char auth[1];
/* Element =
   Description = User defined (ID, FL, ...) */
	char code[2];
/* Element =
   Description = */
	char repno[20];
/* Element = ACCOUNT
   Description = WH Account */
	char wh_acct[18];
/* Element = ACCOUNT
   Description = OST Liability Account */
	char ost_lia_acct[18];
/* Element = ACCOUNT
   Description = SUI Expense Account */
	char sui_ex_acct[18];
/* Element = ACCOUNT
   Description = SUI Liability Account */
	char sui_lia_acct[18];
/* Element =
   Description = SUI Percentage */
	double sui_pct;
/* Element =
   Description = SUI Maximum */
	double sui_max;
/* Element =
   Description = Minimum wage */
	double min_wage;
/* Element = ACCOUNT
   Description = OST Expense account */
	char ost_ex_acct[18];
/* Element =
   Description = OST Percentage */
	double ost_pct;
/* Element =
   Description = OST Maximum */
	double ost_max;
/* Element =
   Description = OST Ded maximum */
	double ost_dedmax;
/* Element =
   Description = SUI Account Number */
	char sutano[20];
};
#pragma member_alignment restore
