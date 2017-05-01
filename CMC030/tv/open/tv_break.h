/*
 * File Layout for: TV.TV_BREAK on 21-May-01
 *
 * TV Break Master File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct tv_break_cdd
{
/* Element =
   Description = Program number */
	char prgnum[10];
/* Element =
   Description = Relative Run time */
	char run_time[6];
/* Element =
   Description = Break Description */
	char descr[30];
/* Element =
   Description = Break Type */
	char brktype[2];
/* Element =
   Description = Break Length */
	char length[6];
/* Element =
   Description = Comment */
	char comment[30];
/* Element =
   Description = Maximum number of commercials */
	int maxcom;
/* Element =
   Description = Priority */
	int priority;
/* Element =
   Description = Match code */
	char match[6];
/* Element =
   Description = Fill (usable for anything) */
	char filler[2];
};
#pragma member_alignment restore
