/*
 * File Layout for: BT.BT_TUITION on 21-May-01
 *
 * Tuition Table
 */

#pragma member_alignment save
#pragma nomember_alignment

struct bt_tuition_cdd
{
/* Element = CUSTOMER
   Description = Guardian */
	char cusnum[10];
/* Element =
   Description = Child */
	char child[40];
/* Element = DATE
   Description = Effective from date */
	char fromdate[8];
/* Element = DATE
   Description = Effective to date */
	char todate[8];
/* Element =
   Description = Monthly Rate */
	double rate;
/* Element = ACCOUNT
   Description = General Ledger Account Number */
	char account[18];
/* Element =
   Description = Monthly Rate */
	double diaper_rate;
};
#pragma member_alignment restore
