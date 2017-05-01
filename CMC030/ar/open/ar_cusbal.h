/*
 * File Layout for: AR.AR_CUSBAL on 21-May-01
 *
 * Customer Balances
 */

#pragma member_alignment save
#pragma nomember_alignment

struct ar_cusbal_cdd
{
/* Element = CUSNUM
   Description = Customer Number */
	char cusnum[10];
/* Element = ACCOUNT
   Description = General Ledger Account Number */
	char acct[18];
/* Element =
   Description = Credit Limit */
	double credit;
/* Element =
   Description = Aging periods */
	double aging[5];
/* Element =
   Description = Future amount */
	double future;
/* Element =
   Description = YTD Service charge */
	double ytdservice;
/* Element =
   Description = PTD Sales */
	double last_paid;
/* Element =
   Description = YTD Sales */
	double ytdsales;
/* Element =
   Description = Service charge */
	double charge;
/* Element =
   Description = Last Service Charge date */
	char last_charge[8];
/* Element =
   Description = Last Payment date */
	char last_payment[8];
/* Element =
   Description = Last update */
	char last_update[8];
};
#pragma member_alignment restore
