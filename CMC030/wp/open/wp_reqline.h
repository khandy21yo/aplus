/*
 * File Layout for: WP.WP_REQLINE on 21-May-01
 *
 * Material Requisition Journal Line
 */

#pragma member_alignment save
#pragma nomember_alignment

struct wp_reqline_cdd
{
/* Element = JOB
   Description = Job number */
	char job[10];
/* Element = LINE
   Description = Job Line */
	char lline[4];
/* Element = REQNUM
   Description = Requisition Number */
	char reqnum[10];
/* Element = OPERATION
   Description = Operation */
	char operation[8];
/* Element = PRODUCT
   Description = Product Number */
	char product[14];
/* Element =
   Description = Quantity Required */
	double qty;
/* Element = REQLINE
   Description = Requistition Line */
	char reqline[4];
};
#pragma member_alignment restore
