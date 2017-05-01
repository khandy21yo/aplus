/*
 * File Layout for: GL.GL_BUD_YYYY on 21-May-01
 *
 * Budget File for Fiscal Year
 */

#pragma member_alignment save
#pragma nomember_alignment

struct gl_bud_yyyy_cdd
{
/* Element = ACCOUNT
   Description = General Ledger account number */
	char acct[18];
/* Element = AMOUNT
   Description = Dollar amount */
	double dollar[14];
/* Element = UNIT
   Description = Unit amount */
	double unit[14];
/* Element =
   Description = Budget hours */
	double hour[14];
};
#pragma member_alignment restore
