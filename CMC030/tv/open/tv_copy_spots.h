/*
 * File Layout for: TV.TV_COPY_SPOTS on 21-May-01
 *
 * TV Copy Spots
 */

#pragma member_alignment save
#pragma nomember_alignment

struct tv_copy_spots_cdd
{
/* Element =
   Description = Form number */
	char frmnum[8];
/* Element =
   Description = Sequential number */
	char seqnum[2];
/* Element =
   Description = Spot number */
	char spot_num[2];
/* Element =
   Description = House cart number */
	char cart_num[10];
/* Element =
   Description = Agency cart number */
	char agency_cart[20];
/* Element =
   Description = Description */
	char descr[20];
};
#pragma member_alignment restore
