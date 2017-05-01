/*
 * File Layout for: PR.PR_WC_DEFINITION on 21-May-01
 *
 * Workman Comp Definition File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct pr_wc_definition_cdd
{
/* Element =
   Description = */
	char code[6];
/* Element = ACCOUNT
   Description = General Ledger Account Number */
	char subj_acct[18];
/* Element = OPERATION
   Description = Operation */
	char oper[8];
};
#pragma member_alignment restore
