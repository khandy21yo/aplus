/*
 * File Layout for: OS.OS_JLINE on 21-May-01
 *
 * Lowest line level journal
 */

#pragma member_alignment save
#pragma nomember_alignment

struct os_jline_cdd
{
/* Element = ORDNUM
   Description = Transaction key */
	char ordnum[10];
/* Element = LINE
   Description = Line */
	char jline[4];
/* Element = LINE
   Description = Line */
	char lline[4];
/* Element = CATEGORY
   Description = Category for this item */
	char category[4];
/* Element = PRODUCT
   Description = Product Number Selected */
	char product[14];
/* Element = QUANTITY
   Description = Quantity */
	double quantity;
/* Element =
   Description = Quoted price */
	double price;
};
#pragma member_alignment restore
