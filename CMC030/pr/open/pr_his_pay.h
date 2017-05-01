/*
 * File Layout for: PR.PR_HIS_PAY on 21-May-01
 *
 * Payroll Pay History
 */

#pragma member_alignment save
#pragma nomember_alignment

struct pr_his_pay_cdd
{
/* Element = EMPNUM
   Description = Employee number */
	char empnum[10];
/* Element = DATE
   Description = Date */
	char pr_end_date[8];
/* Element =
   Description = Employee skill */
	char emp_skill[6];
/* Element =
   Description = Employee Grade */
	char emp_grade[2];
/* Element = ACCOUNT
   Description = General Ledger Account Number */
	char acct[18];
/* Element = SUBACC
   Description = Sub account (job number) */
	char subacc[10];
/* Element = OPERATION
   Description = Operation */
	char oper[8];
/* Element = LOCATION
   Description = Location */
	char location[4];
/* Element =
   Description = Department */
	char dept[6];
/* Element = WORK_CENTER
   Description = Work Center */
	char work_center[4];
/* Element =
   Description = Union code */
	char union[2];
/* Element =
   Description = Pay type (P-time, O-other pay) */
	char ptype[1];
/* Element =
   Description = Rate type (H-hourly, S-salary, P-Piece) */
	char rtype[1];
/* Element =
   Description = Earnings code */
	char code[2];
/* Element =
   Description = Piece Rate */
	double piece_rate;
/* Element =
   Description = Hourly Rate */
	double hour_rate;
/* Element =
   Description = Regular hours */
	double reg_hr;
/* Element =
   Description = Overtime hours */
	double ovt_hr;
/* Element =
   Description = Number of pieces producted */
	double piece;
/* Element =
   Description = Overtime factor */
	int factor;
/* Element =
   Description = Gross Pay */
	double gross;
/* Element =
   Description = Tax package code */
	char tax_pkg[2];
/* Element =
   Description = Batch entry flag */
	char batch_entry[2];
/* Element =
   Description = Update flag */
	int update_flag;
/* Element =
   Description = Seq # for labor performance */
	char seqnum[6];
/* Element = BATCH
   Description = Batch number used for posting */
	char batch[6];
/* Element = DATE
   Description = Date Worked */
	char workdate[8];
/* Element =
   Description = Regular Hours By Day * 100.0 */
	int regular[7];
/* Element =
   Description = Overtime hours by day * 100 */
	int overtime[7];
/* Element = ASSET
   Description = Asset number */
	char equipment[10];
/* Element =
   Description = Hours of use on equipment */
	double equiphour;
};
#pragma member_alignment restore
