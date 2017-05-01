/*
 * File Layout for: PR.PR_TAX_PROFILE_FRI on 21-May-01
 *
 * Payroll Tax Fringe Expense Distribution
 */

#pragma member_alignment save
#pragma nomember_alignment

struct pr_tax_profile_fri_cdd
{
/* Element = TAXTYPE
   Description = Tax type (FI,FU,SU) */
	char tax_type[2];
/* Element = ACCOUNT
   Description = General Ledger Account Number */
	char labor_acct[18];
/* Element = ACCOUNT
   Description = General Ledger Account Number */
	char fri_ex_acct[18];
};
#pragma member_alignment restore
