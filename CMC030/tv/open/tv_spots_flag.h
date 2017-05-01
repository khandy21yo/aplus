/*
 * File Layout for: TV.TV_SPOTS_FLAG on 21-May-01
 *
 * Spots Status Flag Table
 */

#pragma member_alignment save
#pragma nomember_alignment

struct tv_spots_flag_cdd
{
/* Element =
   Description = S-schedule, N-not run, R-run */
	char flag[1];
/* Element =
   Description = Code */
	char code[2];
/* Element =
   Description = Scheduled to run code */
	char schd_run_code[2];
/* Element =
   Description = Code description */
	char descr[30];
};
#pragma member_alignment restore
