/*
 * File Layout for: GL.GL_35HISTORY on 21-May-01
 *
 * General Ledger summary by period
 */

#pragma member_alignment save
#pragma nomember_alignment

struct gl_35history_cdd
{
/* Element = ACCOUNT
   Description = General Ledger Account Number */
	char account[18];
/* Element = PERIOD
   Description = Fiscal year (YYYY) and Cycle (PP) */
	char period[6];
/* Element =
   Description = Beginning Balance for period */
	double dollarbegin;
/* Element =
   Description = Change for period */
	double dollarchange;
/* Element =
   Description = Budget for period */
	double dollarbudget;
/* Element =
   Description = Beginning balance */
	double hourbegin;
/* Element =
   Description = Change for period */
	double hourchange;
/* Element =
   Description = Budget for period */
	double hourbudget;
/* Element =
   Description = Beginning Balance */
	double unitbegin;
/* Element =
   Description = Chenge for period */
	double unitchange;
/* Element =
   Description = Budget for period */
	double unitbudget;
};
#pragma member_alignment restore
