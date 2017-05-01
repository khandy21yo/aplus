/*
 * File Layout for: SB.SB_BALANCE on 21-May-01
 *
 * Subaccount Balance File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct sb_balance_cdd
{
/* Element = SYSTEM
   Description = System name */
	char system[2];
/* Element = SUBACCT
   Description = Sub Account Number */
	char subaccount[10];
/* Element = OPERATION
   Description = */
	char operation[8];
/* Element = ACCOUNT
   Description = GL Account Number */
	char account[18];
/* Element = PERIOD
   Description = YYYYPP */
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
/* Element =
   Description = Beginning Amount */
	double beg_amount;
/* Element =
   Description = Beginning Units */
	double beg_units;
/* Element =
   Description = Beginning Hours */
	double beg_hours;
};
#pragma member_alignment restore
