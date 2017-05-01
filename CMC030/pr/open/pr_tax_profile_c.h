/*
 * File Layout for: PR.PR_TAX_PROFILE_C on 21-May-01
 *
 * Payroll Tax Profile - City
 */

#pragma member_alignment save
#pragma nomember_alignment

struct pr_tax_profile_c_cdd
{
/* Element =
   Description = 'C' for city records */
	char auth[1];
/* Element =
   Description = User defined */
	char code[2];
/* Element =
   Description = */
	char repno[20];
/* Element = ACCOUNT
   Description = General Ledger Account Number */
	char wh_acct[18];
/* Element = ACCOUNT
   Description = General Ledger Account Number */
	char city_lia_acct[18];
};
#pragma member_alignment restore
