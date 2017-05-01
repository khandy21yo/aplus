/*
 * File Layout for: BT.BT_CUSTYP on 21-May-01
 *
 * Customer Type Definition
 */

#pragma member_alignment save
#pragma nomember_alignment

struct bt_custyp_cdd
{
/* Element =
   Description = Customer Type */
	char custyp[2];
/* Element = ACCOUNT
   Description = General Ledger Account Number */
	char ar_acct[18];
/* Element = ACCOUNT
   Description = General Ledger Account Number */
	char rev_mask[18];
};
#pragma member_alignment restore
