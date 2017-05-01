/*
 * File Layout for: PO.PO_REG_LINE on 21-May-01
 *
 * Purchase Order Register Lines
 */

#pragma member_alignment save
#pragma nomember_alignment

struct po_reg_line_cdd
{
/* Element = PO
   Description = Purchase order number */
	char po[10];
/* Element = LINE
   Description = Line */
	char po_line[4];
/* Element = VENDOR
   Description = Vendor Number */
	char vendor[10];
/* Element = LOCATION
   Description = Location number */
	char fromlocation[4];
/* Element = PRODUCT
   Description = Product Number */
	char product[14];
/* Element = UOM
   Description = Unit of measurement */
	char uom[2];
/* Element = DESCRIPTION
   Description = Description */
	char description[40];
/* Element = PO_TYPE
   Description = Purchase Order Type */
	char po_type[2];
/* Element =
   Description = Open/Closed Flag */
	char open_close[1];
/* Element = DATE
   Description = Order Date (YYYYMMDD) */
	char orddate[8];
/* Element = BATCH
   Description = Batch number used for process (post,clos */
	char batch[6];
/* Element = PERIOD
   Description = Fiscal year (YYYY) and Cycle (PP) */
	char period[6];
};
#pragma member_alignment restore
