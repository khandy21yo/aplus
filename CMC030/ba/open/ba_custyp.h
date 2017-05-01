/*
 * File Layout for: BA.BA_CUSTYP on 21-May-01
 *
 * Customer Type Definitions
 */

#pragma member_alignment save
#pragma nomember_alignment

struct ba_custyp_cdd
{
/* Element =
   Description = Customer Type */
	char custyp[2];
/* Element = ACCOUNT
   Description = General Ledger Account Number */
	char ar_acct[18];
/* Element = ACCOUNT
   Description = General Ledger Account Number Mask */
	char rev_mask[18];
/* Element =
   Description = Description */
	char descr[30];
};
#pragma member_alignment restore
