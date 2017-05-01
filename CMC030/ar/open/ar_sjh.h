/*
 * File Layout for: AR.AR_SJH on 21-May-01
 *
 * Sales Journal Header
 */

#pragma member_alignment save
#pragma nomember_alignment

struct ar_sjh_cdd
{
/* Element = INVNUM
   Description = Invoice number */
	char invnum[8];
/* Element = CUSNUM
   Description = Customer Number */
	char cusnum[10];
/* Element =
   Description = Transaction type 01 - Invoice 02 - Cash */
	char tratyp[2];
/* Element =
   Description = Transaction date */
	char tradat[8];
/* Element =
   Description = Transaction amount */
	double amount;
/* Element = ACCOUNT
   Description = General Ledger Account Number */
	char aracct[18];
/* Element =
   Description = Receipt number */
	char recnum[8];
/* Element =
   Description = Check number */
	char check[6];
/* Element =
   Description = Deposit number */
	char deposit[6];
/* Element =
   Description = Description */
	char descr[26];
/* Element = DATE
   Description = Due Date (YYYYMMDD) */
	char duedate[8];
/* Element = DATE
   Description = Discount Date (YYYYMMDD) */
	char discountdate[8];
/* Element = SUBACCT
   Description = Sub account (job number) */
	char subacct[10];
};
#pragma member_alignment restore
