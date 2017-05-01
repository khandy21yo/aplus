/*
 * File Layout for: OS.OS_BOM_LINE on 21-May-01
 *
 * Bill of Material of Signs
 */

#pragma member_alignment save
#pragma nomember_alignment

struct os_bom_line_cdd
{
/* Element = PRODUCT
   Description = Product Number */
	char product[14];
/* Element = CATEGORY
   Description = Category to fill out */
	char category[4];
/* Element = QUANTITY
   Description = Default Quantity */
	double quantity;
/* Element = YESNO
   Description = Multiple entries allowed */
	char multiple[1];
/* Element = YESNO
   Description = At least one required? */
	char required[1];
/* Element = ACCOUNT
   Description = General Ledger Account Number */
	char account[18];
};
#pragma member_alignment restore
