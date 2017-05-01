/*
 * File Layout for: PR.PR_OVERHEAD_DEF on 21-May-01
 *
 * Payroll Overhead Subject File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct pr_overhead_def_cdd
{
/* Element =
   Description = Overhead key */
	char ovh_key[6];
/* Element = ACCOUNT
   Description = General Ledger Account Number */
	char subj_acct[18];
/* Element = OPERATION
   Description = Operation */
	char oper[8];
};
#pragma member_alignment restore
