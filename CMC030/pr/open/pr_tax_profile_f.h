/*
 * File Layout for: PR.PR_TAX_PROFILE_F on 20-Oct-04
 *
 * Payroll Tax Profile - Federal
 */

#pragma member_alignment save
#pragma nomember_alignment

struct pr_tax_profile_f_cdd
{
/* Element =
   Description = 'F' for federal records */
	char auth[1];
/* Element =
   Description = ' ' for federal */
	char code[2];
/* Element =
   Description = */
	char repno[20];
/* Element = ACCOUNT
   Description = WH Account */
	char wh_acct[18];
/* Element = ACCOUNT
   Description = FICA Expense Account */
	char fica_ex_acct[18];
/* Element = ACCOUNT
   Description = FICA Liability Account */
	char fica_lia_acct_empr[18];
/* Element = ACCOUNT
   Description = FICA Employee Liability */
	char fica_lia_acct_empe[18];
/* Element = ACCOUNT
   Description = FUI Expense Account */
	char fui_ex_acct[18];
/* Element = ACCOUNT
   Description = FUI Liability Account */
	char fui_lia_acct[18];
/* Element =
   Description = FUI Percentage */
	double fui_pct;
/* Element =
   Description = FUI Maximum */
	double fui_max;
/* Element = ACCOUNT
   Description = Cash Account */
	char cash_acct[18];
/* Element = ACCOUNT
   Description = PR Accrual Account */
	char pr_accrual_acct[18];
/* Element =
   Description = Minimum wage */
	double min_wage;
/* Element =
   Description = Direct Deposit Code */
	char direct[20];
/* Element =
   Description = Extra Space 1 */
	char extra1[20];
/* Element =
   Description = Extra Space 2 */
	char extra2[20];
};
#pragma member_alignment restore
