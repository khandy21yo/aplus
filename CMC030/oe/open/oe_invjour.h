/*
 * File Layout for: OE.OE_INVJOUR on 21-May-01
 *
 * Order Invoice Journal Header File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct oe_invjour_cdd
{
/* Element =
   Description = Order Number */
	char ordnum[10];
/* Element =
   Description = Invoice Date */
	char invdate[8];
/* Element =
   Description = Date */
	char shipdate[8];
/* Element =
   Description = Terms */
	char terms[2];
/* Element =
   Description = Handling */
	double handling;
/* Element =
   Description = Order Discount Amount */
	double disc;
/* Element =
   Description = Miscellaneous Charges */
	double misc;
/* Element =
   Description = Reason Code */
	char creason[2];
/* Element =
   Description = Number of Payments */
	int paymnt;
/* Element =
   Description = unused field */
	char unused[14];
/* Element =
   Description = Freight Amount */
	double freight;
/* Element =
   Description = Sales Tax Amount */
	double salestax;
/* Element =
   Description = Operator */
	char operator[10];
/* Element =
   Description = Notes */
	char notes[4][40];
/* Element = INVOICE
   Description = Invoice number */
	char invoice[8];
/* Element =
   Description = Packing List Release Number */
	char shipno[2];
/* Element = CARRIER
   Description = Carrier Code (Ship Via) */
	char shipvia[2];
/* Element = ACCOUNT
   Description = General Ledger Account Number */
	char miscacct[18];
};
#pragma member_alignment restore
