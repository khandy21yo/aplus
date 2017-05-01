/*
 * File Layout for: GL.POST_TO_GL on 21-May-01
 *
 * Batch Control File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct post_to_gl_cdd
{
/* Element =
   Description = Account number */
	char acct[18];
/* Element =
   Description = Description from chart */
	char descr[40];
/* Element =
   Description = Account type */
	char acctype[2];
/* Element =
   Description = Real account balance */
	double begbal;
/* Element =
   Description = Amount of credits */
	double credit;
/* Element =
   Description = Amount of debits */
	double debit;
/* Element =
   Description = Number of units */
	double units;
/* Element =
   Description = Number of hours */
	double hours;
/* Element =
   Description = Undefined Account (Y/N) */
	char undefined_acct[1];
};
#pragma member_alignment restore
