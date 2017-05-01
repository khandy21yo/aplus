/*
 * File Layout for: WP.WP_ORDERLINE on 21-May-01
 *
 * Manufacturing Work In Process Production Line File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct wp_orderline_cdd
{
/* Element = JOB
   Description = Job number */
	char job[10];
/* Element =
   Description = Type of Line(M=material,L=labor) */
	char ttype[1];
/* Element = PRODUCT
   Description = Product Number or Operation Code */
	char itemcode[14];
/* Element =
   Description = Cost per Unit Of measure */
	double cost;
/* Element = DESCRIPTION
   Description = Description of product or additional */
	char descr[40];
/* Element =
   Description = Original Qty */
	double qty;
/* Element = DATE
   Description = Date expected to start production */
	char start_date[8];
/* Element = DATE
   Description = Date Production expected to be complete */
	char comp_date[8];
/* Element =
   Description = Line Number */
	char lline[4];
};
#pragma member_alignment restore
