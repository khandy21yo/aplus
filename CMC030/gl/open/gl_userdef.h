/*
 * File Layout for: GL.GL_USERDEF on 21-May-01
 *
 * User Defined GL Journal Definition File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct gl_userdef_cdd
{
/* Element =
   Description = Journal Code */
	char jcode[4];
/* Element =
   Description = Line Number */
	char jline[4];
/* Element = DESCRIPTION
   Description = Description */
	char description[40];
/* Element = ACCOUNT
   Description = General Ledger Account Number */
	char account[18];
/* Element =
   Description = AR/AP Posting flag */
	char arpflag[2];
/* Element = YESNO
   Description = Input units? */
	char unitflag[1];
/* Element = XREF
   Description = Cross Reference */
	char xref[10];
/* Element = YESNO
   Description = Duplicate entries allowed */
	char duplct[1];
/* Element =
   Description = +/- Numeric Sign */
	char signed[1];
};
#pragma member_alignment restore
