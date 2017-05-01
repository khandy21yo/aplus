/*
 * File Layout for: PO.PO_ORDERSLINE on 21-May-01
 *
 * PO Subline Journal File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct po_ordersline_cdd
{
/* Element = PO
   Description = Purchase order number */
	char po[10];
/* Element = LINE
   Description = Line */
	char po_line[4];
/* Element =
   Description = Our Quanity */
	double our_qty;
/* Element = DATE
   Description = Expected Date (YYYYMMDD) */
	char receivedate[8];
/* Element = ACCOUNT
   Description = General Ledger Account Number */
	char gl_account[18];
/* Element = SUBACCT
   Description = Sub account (job number) */
	char subacct[10];
/* Element =
   Description = Notes */
	char notes[2][40];
};
#pragma member_alignment restore
