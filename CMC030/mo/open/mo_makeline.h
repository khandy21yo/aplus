/*
 * File Layout for: MO.MO_MAKELINE on 21-May-01
 *
 * Make Template Line Table
 */

#pragma member_alignment save
#pragma nomember_alignment

struct mo_makeline_cdd
{
/* Element =
   Description = Dealer Model of Make */
	char make[10];
/* Element =
   Description = Beginning Year for Make (YYYY) */
	char year[4];
/* Element =
   Description = Type of Make */
	char mtype[2];
/* Element =
   Description = Size of Make */
	char msize[4];
/* Element =
   Description = Model Code */
	char modelcode[4];
};
#pragma member_alignment restore
