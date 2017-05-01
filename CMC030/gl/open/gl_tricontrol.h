/*
 * File Layout for: GL.GL_TRICONTROL on 21-May-01
 *
 * Tri-Spur Journal Control File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct gl_tricontrol_cdd
{
/* Element = LOCATION
   Description = Location number */
	char location[4];
/* Element = DESCRIPTION
   Description = Description */
	char description[21][20];
/* Element = ACCOUNT
   Description = General Ledger Account Number */
	char account[21][18];
/* Element =
   Description = Flags */
	char flag[21][1];
};
#pragma member_alignment restore
