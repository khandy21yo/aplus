/*
 * File Layout for: CK.CK_CONTROL on 21-May-01
 *
 * Check Control File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct ck_control_cdd
{
/* Element =
   Description = Year */
	char year[4];
/* Element =
   Description = Period */
	int period;
/* Element =
   Description = Flag */
	char flag[1];
};
#pragma member_alignment restore
