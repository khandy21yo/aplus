/*
 * File Layout for: BT.BT_CUSTOMEX on 21-May-01
 *
 * Extra Info for Customer File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct bt_customex_cdd
{
/* Element = CUSTOMER
   Description = Customer Number */
	char cusnum[10];
/* Element =
   Description = Customer Type */
	char custyp[2];
};
#pragma member_alignment restore
