/*
 * File Layout for: IC.IC_BINMAP on 21-May-01
 *
 * Product Bin and Level Location
 */

#pragma member_alignment save
#pragma nomember_alignment

struct ic_binmap_cdd
{
/* Element = PRODUCT
   Description = Product number */
	char product[14];
/* Element = LOCATION
   Description = Location number */
	char location[4];
/* Element =
   Description = Bin location */
	char bin[4][6];
/* Element =
   Description = Safety stock */
	double safety;
/* Element =
   Description = Maximum stock level */
	double maxlevel;
/* Element =
   Description = ABC flag */
	char abc[1];
/* Element =
   Description = Cycle count map */
	char cyclemap[8];
};
#pragma member_alignment restore
