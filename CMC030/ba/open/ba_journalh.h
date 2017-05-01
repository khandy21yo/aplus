/*
 * File Layout for: BA.BA_JOURNALH on 21-May-01
 *
 * Agency Journal Header
 */

#pragma member_alignment save
#pragma nomember_alignment

struct ba_journalh_cdd
{
/* Element =
   Description = Billing Number */
	char billnum[10];
/* Element = CUSTOMER
   Description = Agency Number */
	char cusnum[10];
/* Element = INVOICE
   Description = Invoice number */
	char invnum[8];
/* Element = ACCOUNT
   Description = General Ledger Account Number */
	char acct[18];
/* Element =
   Description = Transaction Date */
	char tradat[8];
/* Element = DATE
   Description = From payroll date (YYYYMMDD) */
	char fromdate[8];
/* Element = DATE
   Description = To payroll date (YYYYMMDD) */
	char todate[8];
/* Element =
   Description = Non productive operations */
	char operations[20];
};
#pragma member_alignment restore
