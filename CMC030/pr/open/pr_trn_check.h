/*
 * File Layout for: PR.PR_TRN_CHECK on 21-May-01
 *
 * Payroll Check Journal
 */

#pragma member_alignment save
#pragma nomember_alignment

struct pr_trn_check_cdd
{
/* Element = EMPNUM
   Description = Employee number */
	char empnum[10];
/* Element = DATE
   Description = Date */
	char pr_end_date[8];
/* Element = CHKNUM
   Description = Check number */
	char check[6];
/* Element = DATE
   Description = Date */
	char check_date[8];
/* Element =
   Description = Pay frequency */
	int payfreq;
/* Element =
   Description = Update flag */
	int update_flag;
/* Element = BATCH
   Description = Batch number used for posting */
	char batch[6];
};
#pragma member_alignment restore
