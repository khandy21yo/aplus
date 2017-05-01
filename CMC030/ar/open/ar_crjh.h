/*
 * File Layout for: AR.AR_CRJH on 21-May-01
 *
 * Cash Receipts Journal (Header)
 */

#pragma member_alignment save
#pragma nomember_alignment

struct ar_crjh_cdd
{
/* Element =
   Description = Receipt number */
	char recnum[8];
/* Element = CUSNUM
   Description = Customer Number */
	char cusnum[10];
/* Element =
   Description = Check Number */
	char check[6];
/* Element =
   Description = Deposit number */
	char deposit[6];
/* Element =
   Description = Transaction date */
	char tradat[8];
/* Element = ACCOUNT
   Description = General Ledger Account Number */
	char acct[18];
/* Element =
   Description = Amount */
	double amnt;
/* Element =
   Description = Transaction Type */
	char tratyp[2];
/* Element =
   Description = Description */
	char descr[25];
};
#pragma member_alignment restore
