/*
 * File Layout for: AD.AD_VERYOLD on 21-May-01
 *
 * Old (RSTS) asset format
 */

#pragma member_alignment save
#pragma nomember_alignment

struct ad_veryold_cdd
{
/* Element = ASSET
   Description = Asset number */
	char asset[10];
/* Element = DESCRIPTION
   Description = Description 1 */
	char descr1[40];
/* Element = DESCRIPTION
   Description = Description 2 */
	char descr2[40];
/* Element = DESCRIPTION
   Description = Description 3 */
	char descr3[40];
/* Element = ACCOUNT
   Description = General Ledger Account Number */
	char account[18];
/* Element = DATE
   Description = Pirchase Date (YYYYMMDD) */
	char purchase[8];
/* Element = DATE
   Description = Disposal Date (YYYYMMDD) */
	char disposal[8];
/* Element = METHOD
   Description = Method */
	char method[2];
/* Element =
   Description = Lifetime */
	long life;
/* Element =
   Description = Cost/Basis */
	double costbasis;
/* Element =
   Description = Salvage */
	double salvage;
/* Element =
   Description = Invest Tax Credit */
	double invest;
/* Element =
   Description = First Year Allowance */
	double firstyear;
/* Element =
   Description = Begining Depreciation */
	double begin;
/* Element =
   Description = this Period Depreciation */
	double thisper;
/* Element =
   Description = Accountants Dep. */
	double accountants;
/* Element =
   Description = ACRS Life */
	long acrslife;
/* Element =
   Description = ACRS Method */
	char acrsmethod[2];
};
#pragma member_alignment restore
