/*
 * File Layout for: PC.PC_DEAL on 02-Oct-01
 *
 * Special deals to customers
 */

#pragma member_alignment save
#pragma nomember_alignment

struct pc_deal_cdd
{
/* Element =
   Description = Deal number */
	char deal[20];
/* Element = CUSTOMER
   Description = Customer Number */
	char customer[10];
/* Element = DATE
   Description = Start Date (YYYYMMDD) */
	char startd[8];
/* Element = DATE
   Description = End Date (YYYYMMDD) */
	char endd[8];
/* Element = DESCRIPTION
   Description = Description */
	char descr[40];
};
#pragma member_alignment restore
