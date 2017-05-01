/*
 * File Layout for: PD.PD_SUBSTITUTE on 21-May-01
 *
 * Substitute Part Numbers
 */

#pragma member_alignment save
#pragma nomember_alignment

struct pd_substitute_cdd
{
/* Element = PRODUCT
   Description = Our Product Number */
	char our_product[14];
/* Element =
   Description = Their part number */
	char their_product[30];
/* Element = VENDOR
   Description = Vendor Number */
	char vendor[10];
};
#pragma member_alignment restore
