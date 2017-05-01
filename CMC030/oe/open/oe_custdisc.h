/*
 * File Layout for: OE.OE_CUSTDISC on 21-May-01
 *
 * Customer Product Discount
 */

#pragma member_alignment save
#pragma nomember_alignment

struct oe_custdisc_cdd
{
/* Element = CUSTOMER
   Description = Customer Number */
	char cusnum[10];
/* Element =
   Description = Product Number */
	char product[20];
/* Element =
   Description = Wildcard Product Type */
	char prodtype[20];
/* Element =
   Description = Wildcard Product Category */
	char prodcat[20];
/* Element = PRICETYPE
   Description = Price type */
	char pricetype[2];
/* Element = DISCOUNT
   Description = Discount percentage */
	double discount;
};
#pragma member_alignment restore
