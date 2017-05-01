/*
 * File Layout for: SA.SA_SALCUST on 21-May-01
 *
 * Customer Sales File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct sa_salcust_cdd
{
/* Element = CUSTOMER
   Description = Customer Number */
	char cusnum[10];
/* Element = ACCOUNT
   Description = General Ledger Account Number */
	char account[18];
/* Element = PERIOD
   Description = Fiscal year (YYYY) and Cycle (PP) */
	char period[6];
/* Element =
   Description = Amount */
	double amount;
/* Element = DATE
   Description = PostDate (YYYYMMDD) */
	char postdate[8];
/* Element = TIME
   Description = Post Time (HHMMSS) */
	char posttime[6];
/* Element = BATCH
   Description = Batch number used for process (post,clos */
	char batch[6];
};
#pragma member_alignment restore
