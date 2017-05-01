/*
 * File Layout for: BT.BT_JOURNALH on 21-May-01
 *
 * Billing Tuition Journal Header
 */

#pragma member_alignment save
#pragma nomember_alignment

struct bt_journalh_cdd
{
/* Element = CUSTOMER
   Description = Customer Number */
	char cusnum[10];
/* Element = DATE
   Description = Transaction Date (YYYYMMDD) */
	char tradat[8];
/* Element = ACCOUNT
   Description = General Ledger Account Number */
	char ar_acct[18];
/* Element = INVOICE
   Description = Invoice number */
	char invnum[8];
};
#pragma member_alignment restore
