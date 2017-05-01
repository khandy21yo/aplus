/*
 * File Layout for: MO.MO_INVLINEOPT on 21-May-01
 *
 * Manufacturing Invoice Line Options File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct mo_invlineopt_cdd
{
/* Element =
   Description = Order Number */
	char ordnum[10];
/* Element =
   Description = Order Line Number */
	char oline[4];
/* Element =
   Description = Order Line Option Number */
	char optline[4];
/* Element =
   Description = Quantity Invoiced (Shipped) */
	double invqty;
/* Element =
   Description = Quantity Cancelled */
	double cancelqty;
/* Element =
   Description = Sales Price Per Unit */
	double price;
/* Element =
   Description = Discount Percentage */
	double discount;
/* Element =
   Description = Unit Cost */
	double cost;
};
#pragma member_alignment restore
