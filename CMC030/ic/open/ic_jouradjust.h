/*
 * File Layout for: IC.IC_JOURADJUST on 21-May-01
 *
 * Inventory Adjustment Journal
 */

#pragma member_alignment save
#pragma nomember_alignment

struct ic_jouradjust_cdd
{
/* Element = LOCATION
   Description = Location number */
	char location[4];
/* Element = PRODUCT
   Description = Product Number */
	char product[14];
/* Element =
   Description = Adjusted quantity */
	double quantity;
/* Element = ACCOUNT
   Description = General Ledger Account Number */
	char account[18];
};
#pragma member_alignment restore
