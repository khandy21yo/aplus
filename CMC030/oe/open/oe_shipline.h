/*
 * File Layout for: OE.OE_SHIPLINE on 21-May-01
 *
 * Shipping Journal Line File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct oe_shipline_cdd
{
/* Element = ORDNUM
   Description = Order Number */
	char ordnum[10];
/* Element =
   Description = Line Number */
	char lin[4];
/* Element =
   Description = Quantity to Ship */
	double shpqty;
/* Element =
   Description = Canceled Quantity */
	double cancelqty;
};
#pragma member_alignment restore
