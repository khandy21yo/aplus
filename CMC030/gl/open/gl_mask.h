/*
 * File Layout for: GL.GL_MASK on 21-May-01
 *
 * General Ledger Mask
 */

#pragma member_alignment save
#pragma nomember_alignment

struct gl_mask_cdd
{
/* Element = ACCOUNT
   Description = General Ledger Account Number */
	char acct_mask[18];
/* Element =
   Description = Description */
	char descr[40];
/* Element =
   Description = Cash Flow code */
	char cash_flow[4];
/* Element =
   Description = Work capital code */
	char work_capt[4];
/* Element =
   Description = Financial type code */
	char fin_type[10];
};
#pragma member_alignment restore
