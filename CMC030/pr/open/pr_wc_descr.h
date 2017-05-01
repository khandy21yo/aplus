/*
 * File Layout for: PR.PR_WC_DESCR on 21-May-01
 *
 * Workman Comp Description File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct pr_wc_descr_cdd
{
/* Element =
   Description = */
	char code[6];
/* Element =
   Description = */
	char descr[20];
/* Element = ACCOUNT
   Description = General Ledger Account Number */
	char lia_acct[18];
/* Element = ACCOUNT
   Description = General Ledger Account Number */
	char ex_acct[18];
};
#pragma member_alignment restore
