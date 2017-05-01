/*
 * File Layout for: AR.AR_OPEN_DIST on 21-May-01
 *
 * Accounts Receivable Open Distribution File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct ar_open_dist_cdd
{
/* Element = INVNUM
   Description = Invoice number */
	char invnum[8];
/* Element = CUSTOMER
   Description = Customer Number */
	char cusnum[10];
/* Element =
   Description = Line number */
	char sline[3];
/* Element = ACCOUNT
   Description = General Ledger Account Number */
	char acct[18];
/* Element = SUBACC
   Description = Sub account (job number) */
	char subacct[10];
/* Element =
   Description = Amount */
	double amount;
/* Element =
   Description = Quanity */
	double qty;
/* Element =
   Description = Line type */
	char ltype[1];
/* Element =
   Description = Tax type */
	char taxtyp[1];
/* Element =
   Description = Description */
	char descr[26];
/* Element = BATCH
   Description = Batch number used for posting */
	char batch[6];
/* Element = PERIOD
   Description = Fiscal year (YYYY) and Cycle (PP) */
	char updated[6];
/* Element =
   Description = Working Staff # */
	char staff_num[10];
/* Element = DATE
   Description = Date (YYYYMMDD) */
	char post_date[8];
/* Element = TIME
   Description = Time (HHMMSS) */
	char post_time[6];
};
#pragma member_alignment restore
