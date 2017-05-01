/*
 * File Layout for: TV.TV_CART_JOUR on 21-May-01
 *
 * Journal of Cuts in Cart
 */

#pragma member_alignment save
#pragma nomember_alignment

struct tv_cart_jour_cdd
{
/* Element = TV_CARTNUM
   Description = Cart number */
	char cartnum[10];
/* Element =
   Description = Cut number */
	char cutnum[2];
/* Element =
   Description = Description of cut */
	char descr[30];
};
#pragma member_alignment restore
