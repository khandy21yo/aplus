/*
 * File Layout for: WP.WP_CLOSELINE on 21-May-01
 *
 * WIP Closing Variance Journal
 */

#pragma member_alignment save
#pragma nomember_alignment

struct wp_closeline_cdd
{
/* Element = JOB
   Description = Job number */
	char job[10];
/* Element =
   Description = Line Flag */
	char lflag[1];
/* Element =
   Description = Variance Class */
	char vclass[4];
/* Element = ACCOUNT
   Description = Variance Account Number */
	char vacct[18];
/* Element =
   Description = Variance Amount */
	double vamount;
};
#pragma member_alignment restore
