/*
 * File Layout for: TV.TV_COMMER_CHARGE on 21-May-01
 *
 * Commercial Charge File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct tv_commer_charge_cdd
{
/* Element =
   Description = Form Number */
	char frmnum[8];
/* Element =
   Description = Billable date */
	char bill_date[8];
/* Element =
   Description = Description */
	char descr[30];
/* Element =
   Description = Dollar Amount */
	double amount;
/* Element = ACCOUNT
   Description = General Ledger Account Number */
	char acctno[18];
/* Element =
   Description = GL period */
	char period[8];
/* Element =
   Description = CO-OP Sponser Number */
	char coop[10];
/* Element =
   Description = Billed flag (Y/N) */
	char bill_flag[1];
/* Element =
   Description = Bill flag (% - percent, $ - dollars) */
	char bill_type[1];
};
#pragma member_alignment restore
