/*
 * File Layout for: GL.GL_OBJECT on 21-May-01
 *
 * General Ledger Object Mask File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct gl_object_cdd
{
/* Element = ACCOUNT
   Description = General Ledger Account Number */
	char obj_mask[18];
/* Element = DESCRIPTION6
   Description = Description */
	char descr[40];
/* Element =
   Description = Account Type */
	char acct_type[1];
/* Element =
   Description = Summary Flag */
	char summ_flag[1];
/* Element =
   Description = Cash Flow */
	char cash_flow[4];
/* Element =
   Description = Work Capitol */
	char work_capt[4];
/* Element =
   Description = Financial Type */
	char fin_type[10];
};
#pragma member_alignment restore
