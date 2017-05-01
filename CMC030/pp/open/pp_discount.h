/*
 * File Layout for: PP.PP_DISCOUNT on 21-May-01
 *
 * Discount Table
 */

#pragma member_alignment save
#pragma nomember_alignment

struct pp_discount_cdd
{
/* Element =
   Description = Discount Code (A,I??) */
	char code[4];
/* Element = DESCRIPTION
   Description = Description */
	char description[40];
/* Element =
   Description = Method (V=Volume,N=Nonvolume) */
	char method[1];
/* Element =
   Description = Over Amount */
	double over[10];
/* Element =
   Description = Rate */
	double rate[10];
};
#pragma member_alignment restore
