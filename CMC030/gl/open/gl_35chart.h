/*
 * File Layout for: GL.GL_35CHART on 21-May-01
 *
 * CHART OF ACCOUNTS
 */

#pragma member_alignment save
#pragma nomember_alignment

struct gl_35chart_cdd
{
/* Element = ACCOUNT
   Description = General Ledger Account Number */
	char account[18];
/* Element =
   Description = DESCRIPTION */
	char descr[40];
/* Element =
   Description = Account type */
	char acctype[1];
/* Element =
   Description = Category */
	char category[4];
/* Element =
   Description = Cash flow */
	char flow[4];
/* Element =
   Description = Work Capitol */
	char work[4];
/* Element =
   Description = Financial type */
	char fintype[10];
/* Element =
   Description = Summary flag */
	char summary[1];
/* Element =
   Description = System id */
	char system[2];
/* Element = ACCOUNT
   Description = Accrual Account */
	char accrual_acct[18];
};
#pragma member_alignment restore
