/*
 * File Layout for: AR.AR_SJL on 21-May-01
 *
 * Sales Journal (Lines)
 */

#pragma member_alignment save
#pragma nomember_alignment

struct ar_sjl_cdd
{
/* Element = INVNUM
   Description = Invoice number */
	char invnum[8];
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
};
#pragma member_alignment restore
