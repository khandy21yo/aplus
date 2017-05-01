/*
 * File Layout for: OE.OE_CREDITLINE on 21-May-01
 *
 * Credit Memo Line File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct oe_creditline_cdd
{
/* Element =
   Description = Memo Number */
	char memonum[8];
/* Element = PRODUCT
   Description = Product Number */
	char product[14];
/* Element =
   Description = Total Credited Quantity */
	double credqty;
/* Element =
   Description = Qty Returned back into Inventory */
	double invqty;
/* Element =
   Description = Sales Price per Unit */
	double price;
/* Element =
   Description = Discount Percentage */
	double discount;
/* Element =
   Description = Unit Cost */
	double cost;
/* Element =
   Description = Promo Amount Off */
	double promo;
/* Element =
   Description = Miscellaneous Charges */
	double misc;
/* Element =
   Description = Reason Code */
	char reason[2];
};
#pragma member_alignment restore
