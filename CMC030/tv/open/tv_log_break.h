/*
 * File Layout for: TV.TV_LOG_BREAK on 21-May-01
 *
 * TV Break Log
 */

#pragma member_alignment save
#pragma nomember_alignment

struct tv_log_break_cdd
{
/* Element =
   Description = Date of break */
	char date[8];
/* Element =
   Description = Scheduled Time of break */
	char sch_time[6];
/* Element =
   Description = Actual run time */
	char run_time[6];
/* Element =
   Description = Break Number */
	char prgnum[10];
/* Element =
   Description = Break Description */
	char descr[30];
/* Element =
   Description = Type of break */
	char brktype[2];
/* Element =
   Description = Length of break */
	char length[6];
/* Element =
   Description = Comment */
	char comment[30];
/* Element =
   Description = Maximum number of commercials */
	int maxcom;
/* Element =
   Description = Break priority */
	int priority;
/* Element =
   Description = Match code */
	char match[6];
/* Element =
   Description = Fill (usable for anything) */
	char filler[2];
/* Element =
   Description = Run (0-yes, 1-no, 2-cancelled) */
	char run[1];
};
#pragma member_alignment restore
