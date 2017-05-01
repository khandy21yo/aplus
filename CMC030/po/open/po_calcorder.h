/*
 * File Layout for: PO.PO_CALCORDER on 21-May-01
 *
 * Calculate PO Order Journal
 */

#pragma member_alignment save
#pragma nomember_alignment

struct po_calcorder_cdd
{
/* Element = VENDOR
   Description = Vendor Number */
	char vendor[10];
/* Element = PRODUCT
   Description = Product Number */
	char product[14];
/* Element = LOCATION
   Description = Location number */
	char location[4];
/* Element =
   Description = 10 Quanities used in calculating order */
	double quanity[11];
/* Element =
   Description = Quanity to order */
	double order;
/* Element =
   Description = Cost from vendor */
	double cost;
};
#pragma member_alignment restore
