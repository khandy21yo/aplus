/*
 * File Layout for: TV.TV_COMMER_SKED_INSTR on 21-May-01
 *
 * Commercial Schedule Instructions
 */

#pragma member_alignment save
#pragma nomember_alignment

struct tv_commer_sked_instr_cdd
{
/* Element = TV_FRMNUM
   Description = Form Number */
	char frmnum[8];
/* Element =
   Description = Schedule number */
	char sked_num[2];
/* Element =
   Description = Start date */
	char start_date[8];
/* Element =
   Description = End date */
	char end_date[8];
/* Element =
   Description = Start time slot */
	char start_time[6];
/* Element =
   Description = End time slot */
	char end_time[6];
/* Element =
   Description = Number of weeks in */
	int in_weeks;
/* Element =
   Description = Number of weeks out */
	int out_weeks;
/* Element =
   Description = Length */
	char length[6];
/* Element =
   Description = Rate per spot */
	double rate_per_spot;
/* Element =
   Description = Spots per day (mon thur sun) */
	int spots_per_day[7];
/* Element =
   Description = Used for rotating spots */
	int total_spots;
};
#pragma member_alignment restore
