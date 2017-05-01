/*
 * File Layout for: GL.GL_PERIOD on 21-May-01
 *
 * General Ledger Period File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct gl_period_cdd
{
/* Element =
   Description = */
	char period[14][20];
/* Element =
   Description = */
	int lastperclo;
/* Element =
   Description = */
	int fpfy;
/* Element =
   Description = */
	char year[4];
/* Element =
   Description = */
	char bthnum[6];
/* Element =
   Description = */
	double summarytotal;
/* Element =
   Description = */
	char summaryacct[18];
/* Element =
   Description = */
	int newyear;
/* Element =
   Description = End date in period */
	char enddate[14][4];
/* Element =
   Description = 1-Closing, 2-Resetting */
	char closeflag[1];
};
#pragma member_alignment restore
