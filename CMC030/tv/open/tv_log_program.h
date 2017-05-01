/*
 * File Layout for: TV.TV_LOG_PROGRAM on 21-May-01
 *
 * TV Program Log
 */

#pragma member_alignment save
#pragma nomember_alignment

struct tv_log_program_cdd
{
/* Element =
   Description = Run date */
	char date[8];
/* Element =
   Description = Run time */
	char start_time[6];
/* Element =
   Description = Actual run time */
	char run_time[6];
/* Element =
   Description = Program number */
	char prgnum[10];
/* Element =
   Description = Title */
	char title[40];
/* Element =
   Description = Source */
	char source[4];
/* Element =
   Description = Type */
	char ptype[4];
/* Element =
   Description = Length */
	char length[6];
/* Element =
   Description = Run (0-yes, 1-no, 2-cancelled) */
	char run[1];
/* Element =
   Description = Comment */
	char comment[50];
/* Element =
   Description = Cutaway flag (Y/N) */
	char cutaway[10];
};
#pragma member_alignment restore
