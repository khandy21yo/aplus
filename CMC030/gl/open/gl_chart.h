/*
 * File Layout for: GL.GL_CHART on 21-May-01
 *
 * Chart of Accounts
 */

#pragma member_alignment save
#pragma nomember_alignment

struct gl_chart_cdd
{
/* Element = ACCOUNT
   Description = General Ledger Account Number */
	char acct[18];
/* Element =
   Description = Description */
	char descr[40];
/* Element =
   Description = Account type */
	char acctype[1];
/* Element =
   Description = Cash flow */
	char flow[4];
/* Element =
   Description = Work capitol */
	char work[4];
/* Element =
   Description = Financial type */
	char fintype[10];
/* Element =
   Description = Summary flag 1 - Detail 2 - By date 3 - */
	char summary[1];
/* Element =
   Description = Period dollar totals */
	double dollar[21];
/* Element =
   Description = Period unit totals */
	double unit[21];
/* Element =
   Description = Period hour totals */
	double hour[21];
/* Element =
   Description = Current period */
	int cperiod;
/* Element =
   Description = Running dollars */
	double rundol;
/* Element =
   Description = Running units */
	double rununit;
/* Element =
   Description = Running hours */
	double runhour;
/* Element =
   Description = Current dollars */
	double curdol;
/* Element =
   Description = Current units */
	double curunit;
/* Element =
   Description = Current hours */
	double curhour;
/* Element =
   Description = Last batch updated */
	char batch[6];
};
#pragma member_alignment restore
