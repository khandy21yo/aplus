/*
 * File Layout for: TV.TV_PROGRAM on 21-May-01
 *
 * TV Program Master File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct tv_program_cdd
{
/* Element =
   Description = Program number */
	char prgnum[10];
/* Element =
   Description = Run time */
	char start_time[7][6];
/* Element =
   Description = From Date */
	char from_date[8];
/* Element =
   Description = To date */
	char to_date[8];
/* Element =
   Description = Program title */
	char title[50];
/* Element =
   Description = Program Source */
	char source[04];
/* Element =
   Description = Program Type */
	char ptype[4];
/* Element =
   Description = Program Length */
	char length[6];
/* Element =
   Description = Comment */
	char comment[50];
/* Element =
   Description = Cutaway flag */
	char cutaway[10];
};
#pragma member_alignment restore
