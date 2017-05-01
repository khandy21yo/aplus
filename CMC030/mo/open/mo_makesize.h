/*
 * File Layout for: MO.MO_MAKESIZE on 21-May-01
 *
 * Make Size Description Table
 */

#pragma member_alignment save
#pragma nomember_alignment

struct mo_makesize_cdd
{
/* Element =
   Description = Make Size Code */
	char msize[4];
/* Element =
   Description = Make Size Code Description */
	char descr[40];
};
#pragma member_alignment restore
