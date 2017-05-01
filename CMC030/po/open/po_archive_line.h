/*
 * File Layout for: PO.PO_ARCHIVE_LINE on 21-May-01
 *
 * Purchase Order Archive Line
 */

#pragma member_alignment save
#pragma nomember_alignment

struct po_archive_line_cdd
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
	char our_product[14];
/* Element = UOM
   Description = Unit of measurement */
	char our_uom[2];
/* Element =
   Description = Our Conversion Factor */
	double our_factor;
/* Element = PRODUCT
   Description = Product Number */
	char ven_product[14];
/* Element = DESCRIPTION
   Description = Description */
	char ven_description[40];
/* Element = UOM
   Description = Unit of measurement */
	char ven_uom[2];
/* Element =
   Description = Vendors conversion factor */
	double ven_factor;
/* Element =
   Description = Discount percentage */
	double ven_discount;
/* Element = ACCOUNT
   Description = General Ledger Account Number */
	char gl_account[18];
/* Element = SUBACCT
   Description = Sub account (job number) */
	char subacct[10];
/* Element = PO_TYPE
   Description = Purchase Order Type */
	char po_type[2];
/* Element =
   Description = Open/Closed Flag */
	char open_close[1];
};
#pragma member_alignment restore
