/*
 * File Layout for: MO.MO_REGLINE on 21-May-01
 *
 * Manufacturing Order Register Line File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct mo_regline_cdd
{
/* Element = ORDNUM
   Description = Order Number */
	char ordnum[10];
/* Element =
   Description = Line Number */
	char lin[4];
/* Element =
   Description = Transaction Type */
	char trantype[2];
/* Element = PRODUCT
   Description = Product Number */
	char product[14];
/* Element =
   Description = Quantity Recorded */
	double qty;
/* Element = DATE
   Description = Transaction Date (YYYYMMDD) */
	char tdate[8];
/* Element =
   Description = Unit Price */
	double price;
/* Element =
   Description = Unit Cost */
	double cost;
/* Element = DATE
   Description = Posting Date (YYYYMMDD) */
	char postdate[8];
/* Element = TIME
   Description = Posting Time (HHMMSS) */
	char posttime[6];
/* Element = BATCH
   Description = Batch number used for process (post,clos */
	char batch[6];
/* Element =
   Description = Packing List Release Number */
	char shipno[2];
/* Element =
   Description = Make of Dealer's Model */
	char make[10];
/* Element =
   Description = Year of Make */
	char year[4];
/* Element =
   Description = Type of Make */
	char mtype[2];
/* Element =
   Description = Size of Make */
	char msize[4];
/* Element =
   Description = Model Code */
	char modelcode[4];
/* Element = REFNUM
   Description = Reference number */
	char refnum[8];
/* Element = NOTES
   Description = Line notes */
	char notes[2][40];
/* Element =
   Description = Discount */
	double discount;
/* Element = PERIOD
   Description = Fiscal year (YYYY) and Cycle (PP) */
	char period[6];
/* Element =
   Description = Serial Num */
	char idnum[10];
};
#pragma member_alignment restore
