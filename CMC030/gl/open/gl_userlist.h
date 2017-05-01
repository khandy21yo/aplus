/*
 * File Layout for: GL.GL_USERLIST on 21-May-01
 *
 * List of Accounts Allowed for Specified Users
 */

#pragma member_alignment save
#pragma nomember_alignment

struct gl_userlist_cdd
{
/* Element =
   Description = User Name */
	char user[16];
/* Element = ACCOUNT
   Description = Allowed General Ledger Account Number */
	char account[18];
/* Element =
   Description = Access Flag */
	char flag[1];
};
#pragma member_alignment restore
