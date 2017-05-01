/*
 * File Layout for: TV.TV_FILL on 21-May-01
 *
 * TV Fill Master File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct tv_fill_cdd
{
/* Element =
   Description = Fill number */
	char filnum[10];
/* Element =
   Description = Description */
	char descr[30];
/* Element =
   Description = Fill class */
	char fclass[4];
/* Element =
   Description = From date */
	char from_date[8];
/* Element =
   Description = To date */
	char to_date[8];
/* Element =
   Description = Length */
	char length[6];
/* Element =
   Description = Number of runs */
	int runs;
/* Element =
   Description = Number of cuts on tape */
	int cuts;
/* Element =
   Description = Current cut */
	int current_cut;
};
#pragma member_alignment restore
