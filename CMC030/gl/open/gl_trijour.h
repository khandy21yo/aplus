/*
 * File Layout for: GL.GL_TRIJOUR on 21-May-01
 *
 * Tri-Spur Journal
 */

#pragma member_alignment save
#pragma nomember_alignment

struct gl_trijour_cdd
{
/* Element = LOCATION
   Description = Location number */
	char location[4];
/* Element = DATE
   Description = Transaction Date (YYYYMMDD) */
	char trandate[8];
/* Element = DOLLAR
   Description = Dollar Amounts */
	double amount[21];
/* Element = ACCOUNT
   Description = General Ledger Account Numbers */
	char account[21][18];
/* Element = DESCRIPTION
   Description = Description */
	char description[21][20];
};
#pragma member_alignment restore
