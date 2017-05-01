/*
 * File Layout for: IC.IC_STOREMAP on 21-May-01
 *
 * Cycle Count Map by Day
 */

#pragma member_alignment save
#pragma nomember_alignment

struct ic_storemap_cdd
{
/* Element = PRODUCT_NUM
   Description = Product number */
	char product_num[14];
/* Element = LOCATION
   Description = Location number */
	char location[4];
/* Element =
   Description = Bin location */
	char bin[4][6];
/* Element =
   Description = ABC flag */
	char abc[1];
/* Element =
   Description = Cycle daily map */
	char cycle_map[12][4];
};
#pragma member_alignment restore
