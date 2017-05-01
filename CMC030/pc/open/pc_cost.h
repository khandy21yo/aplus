/*
 * File Layout for: PC.PC_COST on 21-May-01
 *
 * Product Standard Cost
 */

#pragma member_alignment save
#pragma nomember_alignment

struct pc_cost_cdd
{
/* Element = PRODUCT
   Description = Product number */
	char product[14];
/* Element = LOCATION
   Description = Location number */
	char location[4];
/* Element = DATE
   Description = Effective date (MMDDYYYY) */
	char effdate[8];
/* Element = AMOUNT
   Description = Product standard cost */
	double cost;
};
#pragma member_alignment restore
