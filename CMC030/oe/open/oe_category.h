/*
 * File Layout for: OE.OE_CATEGORY on 21-May-01
 *
 * Sales Order Category Description Table
 */

#pragma member_alignment save
#pragma nomember_alignment

struct oe_category_cdd
{
/* Element =
   Description = Category */
	char ordcat[4];
/* Element =
   Description = Description */
	char description[30];
};
#pragma member_alignment restore
