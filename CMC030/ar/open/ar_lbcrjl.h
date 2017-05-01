/*
 * File Layout for: AR.AR_LBCRJL on 21-May-01
 *
 * Cash Receipts Journal (Line)
 */

#pragma member_alignment save
#pragma nomember_alignment

struct ar_lbcrjl_cdd
{
/* Element =
   Description = Receipt number */
	char recnum[8];
/* Element =
   Description = Line number */
	char lline[3];
/* Element =
   Description = Invoice number */
	char invnum[8];
/* Element = ACCOUNT
   Description = General Ledger Account Number */
	char acct[18];
/* Element =
   Description = Amount */
	double amount;
/* Element =
   Description = Transaction Type */
	char tratyp[1];
/* Element = MATTER
   Description = Matter Number */
	char matter_num[10];
/* Element =
   Description = Flag for Type of allocation */
	char allocate[1];
/* Element =
   Description = Staff Number */
	char staff[10];
};
#pragma member_alignment restore
