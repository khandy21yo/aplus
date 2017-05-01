/*
 * File Layout for: GL.GL_USERHEAD on 21-May-01
 *
 * User Defined Journal Header
 */

#pragma member_alignment save
#pragma nomember_alignment

struct gl_userhead_cdd
{
/* Element =
   Description = Journal Code */
	char jcode[4];
/* Element = DEPOSIT
   Description = Deposit number */
	char deposit[6];
/* Element = DATE
   Description = Journal Date (YYYYMMDD) */
	char jdate[8];
};
#pragma member_alignment restore
