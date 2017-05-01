/*
 * File Layout for: AR.AR_CONTROL_ACCT on 21-May-01
 *
 * Valid Accounts for Accounts Receivable
 */

#pragma member_alignment save
#pragma nomember_alignment

struct ar_control_acct_cdd
{
/* Element = ACCOUNT
   Description = General Ledger Account Number */
	char acct[18];
};
#pragma member_alignment restore
