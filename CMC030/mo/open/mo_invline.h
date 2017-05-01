/*
 * File Layout for: MO.MO_INVLINE on 21-May-01
 *
 * Manufacturing Order Invoice Line Journal File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct mo_invline_cdd
{
/* Element = ORDNUM
   Description = Order Number */
	char ordnum[10];
/* Element =
   Description = Line Number */
	char oline[4];
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
};
#pragma member_alignment restore
