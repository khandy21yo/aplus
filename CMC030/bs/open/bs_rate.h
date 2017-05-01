/*
 * File Layout for: BS.BS_RATE on 21-May-01
 *
 * Rate File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct bs_rate_cdd
{
/* Element =
   Description = */
	char prg[10];
/* Element =
   Description = */
	char rateuom[2];
/* Element =
   Description = */
	char effdate[8];
/* Element =
   Description = */
	double rate;
};
#pragma member_alignment restore
