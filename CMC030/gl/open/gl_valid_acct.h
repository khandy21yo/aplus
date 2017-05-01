/*
 * File Layout for: GL.GL_VALID_ACCT on 21-May-01
 *
 * Valid Account by System Id
 */

#pragma member_alignment save
#pragma nomember_alignment

struct gl_valid_acct_cdd
{
/* Element =
   Description = System id */
	char systemid[6];
/* Element = ACCOUNT
   Description = General Ledger account number */
	char acct[18];
};
#pragma member_alignment restore
