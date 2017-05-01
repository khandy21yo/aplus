/*
 * File Layout for: TV.TV_BREAK_TYPE on 21-May-01
 *
 * TV Break Type File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct tv_break_type_cdd
{
/* Element =
   Description = Break type (LB, SB, NB, etc.) */
	char btype[2];
/* Element =
   Description = Description */
	char descr[20];
/* Element =
   Description = Flag (0-avail, 1-no avail, 2-noavl/int) */
	char bflag[1];
};
#pragma member_alignment restore
