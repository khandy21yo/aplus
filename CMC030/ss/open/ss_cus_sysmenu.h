/*
 * File Layout for: SS.SS_CUS_SYSMENU on 21-May-01
 *
 * System Menu
 */

#pragma member_alignment save
#pragma nomember_alignment

struct ss_cus_sysmenu_cdd
{
/* Element =
   Description = Customer Number */
	char cusnum[10];
/* Element =
   Description = Tape Number */
	char tape[2];
/* Element = DATE
   Description = Installation Date (YYYYMMDD) */
	char insdat[8];
/* Element =
   Description = System Name */
	char system[2];
/* Element =
   Description = System Menu Number */
	char mennum[6];
};
#pragma member_alignment restore
