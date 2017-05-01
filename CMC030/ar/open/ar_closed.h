/*
 * File Layout for: AR.AR_CLOSED on 21-May-01
 *
 * Closed Accounts Receivable Ledger
 */

#pragma member_alignment save
#pragma nomember_alignment

struct ar_closed_cdd
{
/* Element = CUSNUM
   Description = Customer Number */
	char cusnum[10];
/* Element = INVNUM
   Description = Invoice number */
	char invnum[8];
/* Element =
   Description = Transaction Type */
	char tratyp[2];
/* Element =
   Description = Transaction date */
	char tradat[8];
/* Element =
   Description = Sale amount */
	double salamt;
/* Element =
   Description = Discount amount */
	double disamt;
/* Element =
   Description = Other charges (Sales tax) */
	double othchg;
/* Element =
   Description = Receipt number */
	char recnum[8];
/* Element =
   Description = Check number */
	char chknum[6];
/* Element = ACCOUNT
   Description = General Ledger Account Number */
	char aracct[18];
/* Element = SUBACC
   Description = Sub account (job number) */
	char subacc[10];
/* Element =
   Description = Description */
	char descr[25];
/* Element =
   Description = Salesperson number */
	char salnum[10];
/* Element = BATCH
   Description = Batch number used for posting */
	char batch[6];
/* Element =
   Description = (PPYYYY) */
	char updated[6];
/* Element =
   Description = (PPYYYY) */
	char closedate[6];
/* Element = DATE
   Description = Due Date (YYYYMMDD) */
	char duedate[8];
/* Element = DATE
   Description = Date (YYYYMMDD) */
	char discountdate[8];
};
#pragma member_alignment restore
