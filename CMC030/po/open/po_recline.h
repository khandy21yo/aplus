/*
 * File Layout for: PO.PO_RECLINE on 21-May-01
 *
 * Purchase Order Receiver Line Journal
 */

#pragma member_alignment save
#pragma nomember_alignment

struct po_recline_cdd
{
/* Element = PO
   Description = Purchase order number */
	char po[10];
/* Element =
   Description = Purchase Order Line */
	char po_line[4];
/* Element = DESCRIPTION
   Description = Description */
	char description[40];
/* Element = PRODUCT
   Description = Product Number */
	char product[14];
/* Element = UOM
   Description = Unit of measurement */
	char uom[2];
/* Element =
   Description = Receive Quantity */
	double recqty;
/* Element =
   Description = Canceled Quantity */
	double canqty;
/* Element = FLAG
   Description = Line Flag = Y if Line in Register */
	char lineflag[1];
};
#pragma member_alignment restore
