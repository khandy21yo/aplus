/*
 * File Layout for: PR.PR_TAX_PROFILE_E on 21-May-01
 *
 * Payroll Tax Profile - School District
 */

#pragma member_alignment save
#pragma nomember_alignment

struct pr_tax_profile_e_cdd
{
/* Element =
   Description = 'E' for School */
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
	char sch_lia_acct[18];
};
#pragma member_alignment restore
