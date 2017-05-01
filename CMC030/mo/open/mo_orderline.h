/*
 * File Layout for: MO.MO_ORDERLINE on 21-May-01
 *
 * Manufacturing Orders Model Lines
 */

#pragma member_alignment save
#pragma nomember_alignment

struct mo_orderline_cdd
{
/* Element =
   Description = Order Number */
	char ordnum[10];
/* Element =
   Description = Model line Number */
	char lin[4];
/* Element =
   Description = Make of Dealer's Model */
	char make[10];
/* Element =
   Description = Year of Make */
	char year[4];
/* Element =
   Description = Type of Make */
	char mtype[2];
/* Element =
   Description = Size of Make */
	char msize[4];
/* Element =
   Description = Model Code */
	char modelcode[4];
/* Element =
   Description = Inventory Product */
	char product[14];
/* Element =
   Description = Quantity Ordered */
	double ordqty;
/* Element =
   Description = Unit Price */
	double price;
/* Element =
   Description = Unit Cost */
	double cost;
/* Element = DATE
   Description = Date (YYYYMMDD) */
	char reqdate[8];
/* Element =
   Description = Quantity to ship */
	double shpqty;
/* Element =
   Description = Quantity on back order */
	double bckqty;
/* Element = NOTES
   Description = Line notes */
	char notes[2][40];
/* Element =
   Description = Discount */
	double discount;
/* Element =
   Description = Serial Number */
	char idnum[10];
};
#pragma member_alignment restore
