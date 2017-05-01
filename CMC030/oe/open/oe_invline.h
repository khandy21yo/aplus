/*
 * File Layout for: OE.OE_INVLINE on 21-May-01
 *
 * Order Invoice Journal Line File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct oe_invline_cdd
{
/* Element = ORDNUM
   Description = Order Number */
	char ordnum[10];
/* Element =
   Description = Line Number */
	char lin[4];
/* Element =
   Description = Quantity (shipped) Invoiced */
	double invqty;
/* Element =
   Description = Quantity Cancelled */
	double cancelqty;
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
   Description = Misc Line Charges */
	double misch;
/* Element =
   Description = Line Notes */
	char notes[40];
};
#pragma member_alignment restore
