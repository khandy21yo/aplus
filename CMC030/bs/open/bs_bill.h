/*
 * File Layout for: BS.BS_BILL on 21-May-01
 *
 * Bill File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct bs_bill_cdd
{
/* Element =
   Description = */
	char client[10];
/* Element =
   Description = */
	char prg[10];
/* Element =
   Description = */
	char rateuom[2];
/* Element =
   Description = */
	double rate;
/* Element =
   Description = */
	double length;
/* Element =
   Description = */
	double amount;
/* Element =
   Description = */
	char initials[3];
};
#pragma member_alignment restore
