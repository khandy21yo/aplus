/*
 * File Layout for: PO.PO_TYPE on 21-May-01
 *
 * Purchase Order Type
 */

#pragma member_alignment save
#pragma nomember_alignment

struct po_type_cdd
{
/* Element =
   Description = PO Type */
	char potype[2];
/* Element =
   Description = Description */
	char descr[40];
};
#pragma member_alignment restore
