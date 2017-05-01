/*
 * File Layout for: OE.OE_ORDERTYPE on 21-May-01
 *
 * Sales Order Type Description Table
 */

#pragma member_alignment save
#pragma nomember_alignment

struct oe_ordertype_cdd
{
/* Element =
   Description = Order Type */
	char ordtype[2];
/* Element =
   Description = Description */
	char description[30];
};
#pragma member_alignment restore
