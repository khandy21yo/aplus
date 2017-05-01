/*
 * File Layout for: GL.GL_VENCOL on 21-May-01
 *
 * User Defined Report Table
 */

#pragma member_alignment save
#pragma nomember_alignment

struct gl_vencol_cdd
{
/* Element =
   Description = Record Key */
	char reckey[8];
/* Element = PERIOD
   Description = Fiscal year (YYYY) and Cycle (PP) */
	char fromper[6];
/* Element =
   Description = Column Title "A" */
	char col_titlea[11][12];
/* Element =
   Description = Column Title "B" */
	char col_titleb[11][12];
/* Element =
   Description = Account Wildcard */
	char col_account[11][45];
/* Element =
   Description = Store/Detail Code Flag */
	char col_flag[1];
/* Element =
   Description = Title */
	char col_title[40];
};
#pragma member_alignment restore
