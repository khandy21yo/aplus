/*
 * File Layout for: BA.BA_BILTBL on 21-May-01
 *
 * Employee Billing
 */

#pragma member_alignment save
#pragma nomember_alignment

struct ba_biltbl_cdd
{
/* Element = EMPLOYEE
   Description = Employee number */
	char empnum[10];
/* Element = CUSTOMER
   Description = Customer Number */
	char cusnum[10];
/* Element =
   Description = Contract number */
	char contract[20];
/* Element = DATE
   Description = From Date (YYYYMMDD) */
	char fromdate[8];
/* Element = DATE
   Description = To Date (YYYYMMDD) */
	char todate[8];
/* Element =
   Description = Method */
	char method[1];
/* Element =
   Description = Rate */
	double rate;
/* Element =
   Description = Amount Billable */
	double billable;
/* Element =
   Description = Amount Billed To date */
	double biltodat;
/* Element = BATCH
   Description = Batch number used for process (post,clos */
	char batch[6];
};
#pragma member_alignment restore
