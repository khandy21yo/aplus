/*
 * File Layout for: PO.PO_RECJOUR on 21-May-01
 *
 * PO Receiver Journal Header
 */

#pragma member_alignment save
#pragma nomember_alignment

struct po_recjour_cdd
{
/* Element = PO
   Description = Purchase order number */
	char po[10];
/* Element = DATE
   Description = Receive Date (YYYYMMDD) */
	char recdate[8];
/* Element = REFNO
   Description = Reference number */
	char refno[16];
/* Element = OPERATOR
   Description = Operator */
	char operator[10];
};
#pragma member_alignment restore
