/*
 * File Layout for: PP.PP_TRANTYPE on 21-May-01
 *
 * Pacific Pride Transaction Type
 */

#pragma member_alignment save
#pragma nomember_alignment

struct pp_trantype_cdd
{
/* Element =
   Description = Transaction Type */
	char trantype[2];
/* Element =
   Description = Transaction Description */
	char description[30];
};
#pragma member_alignment restore
