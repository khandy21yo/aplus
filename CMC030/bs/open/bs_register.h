/*
 * File Layout for: BS.BS_REGISTER on 21-May-01
 *
 * Register File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct bs_register_cdd
{
/* Element =
   Description = */
	char client[10];
/* Element =
   Description = */
	char prg[10];
/* Element =
   Description = */
	char period[6];
/* Element =
   Description = */
	char rateuom[2];
/* Element =
   Description = */
	char initials[3];
/* Element =
   Description = */
	double length;
/* Element =
   Description = */
	double amount;
/* Element =
   Description = */
	char postdate[8];
/* Element =
   Description = */
	char posttime[6];
/* Element =
   Description = */
	char batch[6];
};
#pragma member_alignment restore
