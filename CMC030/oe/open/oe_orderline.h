/*
 * File Layout for: OE.OE_ORDERLINE on 21-May-01
 *
 * Sales Order Line File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct oe_orderline_cdd
{
/* Element = ORDNUM
   Description = Order Number */
	char ordnum[10];
/* Element = PRODUCT
   Description = Product Number */
	char product[14];
/* Element =
   Description = Quantity Ordered */
	double ordqty;
/* Element =
   Description = Quantity to Ship */
	double shpqty;
/* Element =
   Description = Unit Price */
	double price;
/* Element =
   Description = Discount Percentage */
	double discount;
/* Element =
   Description = Unit Cost */
	double cost;
/* Element = DATE
   Description = Request Date (YYYYMMDD) */
	char reqdate[8];
/* Element =
   Description = Promo Amount */
	double promo;
/* Element =
   Description = Miscellaneous Charges */
	double misch;
/* Element =
   Description = Quantity on Backorder */
	double bckqty;
/* Element = NOTES
   Description = Notes */
	char notes1[40];
/* Element = NOTES
   Description = Notes */
	char notes2[30];
/* Element = SUBACCT
   Description = Sub account (job number)/Serial Number */
	char subacct[10];
/* Element = LINE
   Description = Line number */
	char lin[4];
/* Element =
   Description = Miscellaneous Charges (2) */
	double misch2;
};
#pragma member_alignment restore
