/*
 * File Layout for: OS.OS_JMAIN on 21-May-01
 *
 * Main Part Screen
 */

#pragma member_alignment save
#pragma nomember_alignment

struct os_jmain_cdd
{
/* Element = TRANKEY
   Description = Transaction key */
	char ordnum[10];
/* Element = LINE
   Description = Line */
	char jline[4];
/* Element =
   Description = Class */
	char class[8];
/* Element = PRODUCT
   Description = Product Number */
	char product[14];
/* Element = QUANTITY
   Description = Quantity */
	double quantity;
/* Element =
   Description = Qty Invoiced */
	double invoiced;
/* Element =
   Description = Qty Completed */
	double completed;
/* Element = PRICE
   Description = Price Per */
	double price;
/* Element = DESCRIPTION
   Description = Description */
	char description[40];
};
#pragma member_alignment restore
