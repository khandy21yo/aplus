/*
 * File Layout for: TV.TV_CART_INVENTORY on 21-May-01
 *
 * TV Cart Inventory Master File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct tv_cart_inventory_cdd
{
/* Element =
   Description = House Cart number */
	char cartnum[10];
/* Element =
   Description = Production name */
	char proname[40];
/* Element =
   Description = Customer name */
	char cusnum[10];
/* Element =
   Description = Agency number */
	char agency_num[10];
/* Element =
   Description = Title */
	char title[40];
/* Element =
   Description = Length */
	char length[6];
/* Element =
   Description = Date in */
	char date_in[8];
/* Element =
   Description = Date out */
	char date_out[8];
};
#pragma member_alignment restore
