/*
 * File Layout for: AP.AP_CONTROL_ACCOUNT on 21-May-01
 *
 * List of AP accounts for audit reports
 */

#pragma member_alignment save
#pragma nomember_alignment

struct ap_control_account_cdd
{
/* Element = ACCOUNT
   Description = General Ledger Account Number */
	char account[18];
};
#pragma member_alignment restore
