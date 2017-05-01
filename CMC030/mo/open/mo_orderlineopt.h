/*
 * File Layout for: MO.MO_ORDERLINEOPT on 21-May-01
 *
 * Manufacturing Order Line File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct mo_orderlineopt_cdd
{
/* Element =
   Description = Order Number */
	char ordnum[10];
/* Element =
   Description = Model record line number */
	char lin[4];
/* Element =
   Description = Option Group */
	char optgroup[2];
/* Element =
   Description = Option Code */
	char optn[4];
/* Element =
   Description = Order Quantity */
	double ordqty;
/* Element =
   Description = Cost Per Unit */
	double cost;
/* Element =
   Description = Price Per Unit */
	double price;
/* Element = PRODUCT
   Description = Product Number */
	char product[14];
/* Element =
   Description = Option Description */
	char optdescr[40];
/* Element =
   Description = Option Line number */
	char linopt[4];
/* Element =
   Description = Quantity to ship */
	double shpqty;
/* Element =
   Description = Quantity on backorder */
	double bckqty;
/* Element = MAKE
   Description = Make */
	char make[10];
/* Element =
   Description = Model Code */
	char modelcode[4];
};
#pragma member_alignment restore
