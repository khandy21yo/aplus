/*
 * File Layout for: PO.PO_PARTCROSS on 21-May-01
 *
 * Vendor Part Cross Reference
 */

#pragma member_alignment save
#pragma nomember_alignment

struct po_partcross_cdd
{
/* Element = PRODUCT
   Description = Product Number */
	char product[14];
/* Element = VENDOR
   Description = Vendor Number */
	char vendor[10];
/* Element = PRODUCT
   Description = Product Number */
	char venprod[14];
/* Element = UOM
   Description = Unit of measurement */
	char venuom[2];
/* Element =
   Description = Vendor Conversion Factor */
	double venfac;
/* Element =
   Description = Our conversion vactor */
	double factor;
/* Element =
   Description = Product Description */
	char descr[40];
/* Element =
   Description = Lead Time */
	int lead;
/* Element =
   Description = Minimum order quantity */
	double minqty;
/* Element =
   Description = Priority Code */
	char priority[1];
};
#pragma member_alignment restore
