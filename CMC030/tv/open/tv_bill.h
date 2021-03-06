/*
 * File Layout for: TV.TV_BILL on 21-May-01
 *
 * Billing File for TV
 */

#pragma member_alignment save
#pragma nomember_alignment

struct tv_bill_cdd
{
/* Element =
   Description = Customer number */
	char cusnum[10];
/* Element =
   Description = Form number */
	char frmnum[8];
/* Element =
   Description = Schedule number */
	char skednum[2];
/* Element =
   Description = Date */
	char sch_date[8];
/* Element =
   Description = Time */
	char sch_time[6];
/* Element =
   Description = Rate */
	double rate;
/* Element =
   Description = Type (0-commercial, 1-fill) */
	char sch_type[1];
/* Element =
   Description = Spots flag (N-not run, R-run, S-sch) */
	char spots_flag[1];
/* Element =
   Description = Spots code (MG-make good, etc.) */
	char spots_code[2];
/* Element =
   Description = Length */
	char length[6];
/* Element =
   Description = Agency number */
	char agency_num[20];
/* Element =
   Description = Cart number */
	char cartnum[10];
/* Element =
   Description = Cut number */
	char cutnum[2];
/* Element =
   Description = From time slot */
	char from_time_slot[8];
/* Element =
   Description = To time slot */
	char to_time_slot[8];
/* Element =
   Description = Invoice number */
	char invnum[8];
/* Element =
   Description = Invoice date */
	char invdat[8];
/* Element =
   Description = Post date */
	char postdate[8];
/* Element =
   Description = Actual run time */
	char run_time[6];
/* Element =
   Description = Log class */
	char class[4];
/* Element =
   Description = Conflict code */
	char conflict[8];
/* Element =
   Description = Description/comment */
	char descr[30];
/* Element = ACCOUNT
   Description = General Ledger Account Number */
	char gl_acct[18];
/* Element = PERIOD
   Description = Fiscal year (YYYY) and Cycle (PP) */
	char gl_update[6];
/* Element = BATCH
   Description = Batch number used for posting */
	char gl_batch[6];
/* Element =
   Description = Amount billed for */
	double bill_amount;
};
#pragma member_alignment restore
