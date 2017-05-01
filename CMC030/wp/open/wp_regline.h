/*
 * File Layout for: WP.WP_REGLINE on 21-May-01
 *
 * Manufacturing Work In Process Register Line File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct wp_regline_cdd
{
/* Element = JOB
   Description = Job number */
	char job[10];
/* Element = LINE
   Description = Line Number */
	char lline[4];
/* Element =
   Description = Record Type(01-order,02-comp,03-cancel) */
	char rec_type[2];
/* Element =
   Description = Trans Type(M=material,L=labor) */
	char ttype[1];
/* Element =
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
/* Element = BATCH
   Description = Batch No */
	char batch[6];
/* Element = TIME
   Description = Post Time */
	char post_time[6];
/* Element = DATE
   Description = Post Date */
	char post_date[8];
};
#pragma member_alignment restore
