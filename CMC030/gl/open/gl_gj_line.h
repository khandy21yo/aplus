/*
 * File Layout for: GL.GL_GJ_LINE on 21-May-01
 *
 * General Journal Line File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct gl_gj_line_cdd
{
/* Element =
   Description = */
	char journal[6];
/* Element =
   Description = */
	char itemnum[4];
/* Element =
   Description = */
	char acct[18];
/* Element =
   Description = */
	char source[4];
/* Element =
   Description = */
	char descr[30];
/* Element =
   Description = */
	char trandat[8];
/* Element =
   Description = */
	double amount;
/* Element =
   Description = */
	char ckno[6];
/* Element =
   Description = */
	char xrefno[10];
/* Element =
   Description = */
	char trankey[6];
/* Element =
   Description = */
	char subacc[10];
/* Element =
   Description = */
	char operation[8];
/* Element =
   Description = */
	double units;
/* Element =
   Description = */
	double hours;
/* Element =
   Description = */
	char postim[6];
/* Element =
   Description = */
	char posdat[8];
/* Element =
   Description = */
	char batch[6];
};
#pragma member_alignment restore
