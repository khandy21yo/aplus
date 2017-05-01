/*
 * File Layout for: AR.AR_LBCRJH on 21-May-01
 *
 * Cash Receipts Journal
 */

#pragma member_alignment save
#pragma nomember_alignment

struct ar_lbcrjh_cdd
{
/* Element =
   Description = Receipt Number */
	char recnum[8];
/* Element = CUSTOMER
   Description = Customer Number */
	char cusnum[10];
/* Element = CHECK
   Description = Check number */
	char check[6];
/* Element = DEPOSIT
   Description = Deposit number */
	char deposit[6];
/* Element =
   Description = Transaction Date */
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
/* Element = INVOICE
   Description = Apply to first Invoice Number */
	char invoice[8];
};
#pragma member_alignment restore
