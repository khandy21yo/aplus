/*
 * File Layout for: IC.IC_JOURCOUNT on 21-May-01
 *
 * Inventory Cycle Count Entry Journal
 */

#pragma member_alignment save
#pragma nomember_alignment

struct ic_jourcount_cdd
{
/* Element = LOCATION
   Description = Location number */
	char location[4];
/* Element = PRODUCT
   Description = Product Number */
	char product[14];
/* Element =
   Description = Quantity count */
	double quantity;
/* Element =
   Description = Cycle Count Control Number */
	char control[6];
};
#pragma member_alignment restore
