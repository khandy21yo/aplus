/*
 * File Layout for: PO.PO_CONTROL on 21-May-01
 *
 * Purchase Order Controling File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct po_control_cdd
{
/* Element =
   Description = Last Purchase Order number */
	char last_po[10];
/* Element =
   Description = Formula for calculating re-order qty */
	char load_formula[10];
};
#pragma member_alignment restore
