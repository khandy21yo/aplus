/*
 * File Layout for: PO.PO_ORDERLINE on 21-May-01
 *
 * PO journal line file
 */

#pragma member_alignment save
#pragma nomember_alignment

struct po_orderline_cdd
{
/* Element = PO
   Description = Purchase order number */
	char po[10];
/* Element = LINE
   Description = Line */
	char po_line[4];
/* Element = PRODUCT
   Description = Our Product Number */
	char our_product[14];
/* Element = UOM
   Description = Our Unit of measurement */
	char our_uom[2];
/* Element = PRODUCT
   Description = Vendors Product Number */
	char ven_product[14];
/* Element = DESCRIPTION
   Description = Product Description */
	char description[40];
/* Element =
   Description = Expected price */
	double ven_price;
};
#pragma member_alignment restore
