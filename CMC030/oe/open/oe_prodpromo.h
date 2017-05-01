/*
 * File Layout for: OE.OE_PRODPROMO on 21-May-01
 *
 * Product Promotion
 */

#pragma member_alignment save
#pragma nomember_alignment

struct oe_prodpromo_cdd
{
/* Element = PRODUCT
   Description = Product Number */
	char product[14];
/* Element = REFNO
   Description = Reference number */
	char refpromo[16];
/* Element = CUSTOMER
   Description = Customer Number */
	char customer[10];
/* Element = CUSTYPE
   Description = Customer Type */
	char custype[2];
/* Element =
   Description = Customer Category */
	char custcat[4];
/* Element = SALESMAN
   Description = Salesperson number */
	char salesman[10];
/* Element =
   Description = Promo Dollar Amount */
	double promodoll;
/* Element =
   Description = Promo Percentage */
	double promoperc;
};
#pragma member_alignment restore
