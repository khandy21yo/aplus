/*
 * File Layout for: TV.TV_BILTYP on 21-May-01
 *
 * TV Billing Type
 */

#pragma member_alignment save
#pragma nomember_alignment

struct tv_biltyp_cdd
{
/* Element =
   Description = Bill type */
	char btype[2];
/* Element =
   Description = Description */
	char descr[30];
};
#pragma member_alignment restore
