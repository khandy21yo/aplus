/*
 * File Layout for: OE.OE_REGLINE on 21-May-01
 *
 * Sales Order Register Line File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct oe_regline_cdd
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
   Description = Discount Percentage */
	double discount;
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
   Description = Promo Amount */
	double promo;
/* Element = REFNUM
   Description = Reference Number,Invoice Number */
	char refnum[8];
/* Element =
   Description = Miscellaneous Charges */
	double misch;
/* Element = PERIOD
   Description = Fiscal year (YYYY) and Cycle (PP) */
	char period[6];
/* Element = NOTES
   Description = Notes */
	char notes1[40];
/* Element = NOTES
   Description = Notes */
	char notes2[30];
/* Element = SUBACCT
   Description = Sub account (job number)/Serail Number */
	char subacct[10];
/* Element =
   Description = Miscellaneous Charges (2) */
	double misch2;
};
#pragma member_alignment restore
