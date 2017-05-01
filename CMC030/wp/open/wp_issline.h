/*
 * File Layout for: WP.WP_ISSLINE on 21-May-01
 *
 * Material Issue Line File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct wp_issline_cdd
{
/* Element = REQNUM
   Description = Requisition Number */
	char reqnum[10];
/* Element = JOB
   Description = Job number */
	char job[10];
/* Element =
   Description = Job Line Number */
	char lline[4];
/* Element =
   Description = Requisition Line Number */
	char reqline[4];
/* Element = PRODUCT
   Description = Product Number */
	char product[14];
/* Element =
   Description = Product Cost */
	double cost;
/* Element =
   Description = Quantity Issued */
	double qtyissue;
/* Element =
   Description = Quantity Canceled */
	double qtycancel;
/* Element = DATE
   Description = Issue Date (YYYYMMDD) */
	char issdate[8];
/* Element =
   Description = Running Quantity */
	double qtyrun;
/* Element =
   Description = Product Flag */
	char prod_flag[1];
};
#pragma member_alignment restore
