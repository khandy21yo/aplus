/*
 * File Layout for: GL.GL_CHARTEX on 21-May-01
 *
 * System GL Acoounts Numbers
 */

#pragma member_alignment save
#pragma nomember_alignment

struct gl_chartex_cdd
{
/* Element = ACCOUNT
   Description = General Ledger Account Number */
	char account[18];
/* Element =
   Description = System ID */
	char system[2];
/* Element = CATEGORY
   Description = Category */
	char category[4];
};
#pragma member_alignment restore
