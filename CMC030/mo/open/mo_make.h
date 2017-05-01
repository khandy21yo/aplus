/*
 * File Layout for: MO.MO_MAKE on 21-May-01
 *
 * Make Master Table File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct mo_make_cdd
{
/* Element =
   Description = Dealer Model of Make */
	char make[10];
/* Element =
   Description = Make Description */
	char descr[40];
/* Element =
   Description = Year for Make YYYY */
	char year[4];
/* Element = MTYPE
   Description = Type of Make */
	char mtype[2];
/* Element =
   Description = Size of the Make */
	char msize[4];
/* Element = CLASS
   Description = Class */
	char class[4];
/* Element =
   Description = Cut Tubing in inches ie: xx x/x */
	char tubing[8];
/* Element =
   Description = Front Slant in Degrees ie: 3.5 */
	char slant[8];
/* Element =
   Description = Overall Length of Cab in inches */
	char overall[8];
/* Element =
   Description = Indicator for Narrow Front (Y/N) */
	char nfront[1];
/* Element =
   Description = Indicator for Narrow Back (Y/N) */
	char nback[1];
};
#pragma member_alignment restore
