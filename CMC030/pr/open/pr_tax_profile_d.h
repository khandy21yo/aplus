/*
 * File Layout for: PR.PR_TAX_PROFILE_D on 21-May-01
 *
 * Payroll Tax Profile - County
 */

#pragma member_alignment save
#pragma nomember_alignment

struct pr_tax_profile_d_cdd
{
/* Element =
   Description = 'D' county authority */
	char auth[1];
/* Element =
   Description = Code */
	char code[2];
/* Element =
   Description = Report number */
	char repno[20];
/* Element = ACCOUNT
   Description = General Ledger Account Number */
	char wh_acct[18];
/* Element = ACCOUNT
   Description = General Ledger Account Number */
	char cou_lia_acct[18];
};
#pragma member_alignment restore
