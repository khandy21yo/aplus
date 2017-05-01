/*
 * File Layout for: SB.SB_BUDGET on 21-May-01
 *
 * Subaccount Budget File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct sb_budget_cdd
{
/* Element =
   Description = System name */
	char system[2];
/* Element = SUBACCT
   Description = Sub account (job number) */
	char subaccount[10];
/* Element = OPERATION
   Description = Operation */
	char operation[8];
/* Element = ACCOUNT
   Description = General Ledger Account Number */
	char account[18];
/* Element = PERIOD
   Description = Fiscal year (YYYY) and Cycle (PP) */
	char period[6];
/* Element =
   Description = Amount */
	double amount;
/* Element =
   Description = Units */
	double units;
/* Element =
   Description = Hours */
	double hours;
};
#pragma member_alignment restore
