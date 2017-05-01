/*
 * File Layout for: UTL.UTL_CARRIER on 21-May-01
 *
 * Table of Carriers (Ship-via)
 */

#pragma member_alignment save
#pragma nomember_alignment

struct utl_carrier_cdd
{
/* Element =
   Description = Carrier Code */
	char code[2];
/* Element =
   Description = Description */
	char descr[40];
};
#pragma member_alignment restore
