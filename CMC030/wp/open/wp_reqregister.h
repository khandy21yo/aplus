/*
 * File Layout for: WP.WP_REQREGISTER on 21-May-01
 *
 * Material Requisition Register File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct wp_reqregister_cdd
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
/* Element = LINE
   Description = Requisition Line */
	char reqlin[4];
/* Element =
   Description = Record Type */
	char rectyp[2];
/* Element = PRODUCT
   Description = Product Number */
	char product[14];
/* Element = LOCATION
   Description = Location number */
	char location[4];
/* Element = QUANTITY
   Description = Quantity */
	double qty;
/* Element =
   Description = Dollar Amount */
	double amt;
/* Element = DATE
   Description = Transactin Date (YYYYMMDD) */
	char trandate[8];
/* Element = OPERATOR
   Description = Operator */
	char operator[10];
/* Element = PERIOD
   Description = Fiscal year (YYYY) and Cycle (PP) */
	char period[6];
/* Element = DATE
   Description = Posting Date (YYYYMMDD) */
	char postdate[8];
/* Element = TIME
   Description = Post Time (HHMMSS) */
	char posttime[6];
/* Element = BATCH
   Description = Batch number used for process (post,clos */
	char batch[6];
};
#pragma member_alignment restore
