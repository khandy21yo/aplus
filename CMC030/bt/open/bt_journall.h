/*
 * File Layout for: BT.BT_JOURNALL on 21-May-01
 *
 * Billing to Agency Journal Lines
 */

#pragma member_alignment save
#pragma nomember_alignment

struct bt_journall_cdd
{
/* Element = CUSTOMER
   Description = Guardian */
	char cusnum[10];
/* Element =
   Description = Child */
	char child[40];
/* Element =
   Description = Rate */
	double rate;
/* Element = ACCOUNT
   Description = General Ledger Account Number */
	char acct[18];
};
#pragma member_alignment restore
