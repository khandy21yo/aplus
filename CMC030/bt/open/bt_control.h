/*
 * File Layout for: BT.BT_CONTROL on 21-May-01
 *
 * Billing Tuition Control File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct bt_control_cdd
{
/* Element =
   Description = Last period closed */
	int lastperclose;
/* Element = YEAR
   Description = Physical year (YYYY) */
	char year[4];
/* Element = INVOICE
   Description = Invoice number */
	char inv_num[8];
/* Element = FLAG
   Description = Flag */
	char flag[1];
};
#pragma member_alignment restore
