/*
 * File Layout for: OE.OE_SHIPJOUR on 21-May-01
 *
 * Shipping Journal Header File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct oe_shipjour_cdd
{
/* Element = ORDNUM
   Description = Order Number */
	char ordnum[10];
/* Element = DATE
   Description = Shipping Date (YYYYMMDD) */
	char shipdate[8];
/* Element = CARRIER
   Description = Carrier Code (Ship Via) */
	char shipvia[2];
/* Element = OPERATOR
   Description = Operator */
	char operator[10];
/* Element =
   Description = Notes */
	char notes[4][40];
/* Element = SHIPNO
   Description = Packing List Release Number */
	char shipno[2];
};
#pragma member_alignment restore
