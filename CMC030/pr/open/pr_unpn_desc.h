/*
 * File Layout for: PR.PR_UNPN_DESC on 21-May-01
 *
 * Payroll Union Pension Description
 */

#pragma member_alignment save
#pragma nomember_alignment

struct pr_unpn_desc_cdd
{
/* Element =
   Description = UNION PENSION CODE */
	char code[2];
/* Element =
   Description = Description */
	char descr[30];
/* Element = ACCOUNT
   Description = General Ledger Account Number */
	char ex_acct[18];
};
#pragma member_alignment restore
