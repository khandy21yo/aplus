/*
 * File Layout for: PR.PR_CONTROL on 21-May-01
 *
 * Payroll Control File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct pr_control_cdd
{
/* Element =
   Description = Year of last purge */
	char year[4];
/* Element = DATE
   Description = Last folder Date Posted */
	char post_date[8];
/* Element = DATE
   Description = Last update/reverse Date */
	char ur_date[8];
/* Element =
   Description = Update/reverse counter */
	int ur_count;
/* Element =
   Description = Close flag */
	char closeflag[1];
/* Element = FLAG
   Description = Apply oh to Department or Subacct (D/S) */
	char oh_apply_flag[1];
};
#pragma member_alignment restore
