/*
 * File Layout for: IC.IC_BALANCE on 21-May-01
 *
 * Product Balance File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct ic_balance_cdd
{
/* Element = PRODUCT
   Description = Product Number */
	char product[14];
/* Element = LOCATION
   Description = Location number */
	char location[4];
/* Element = PERIOD
   Description = Fiscal year (YYYY) and Cycle (PP) */
	char period[6];
/* Element =
   Description = Record type */
	char rectype[2];
/* Element =
   Description = Transaction class */
	char class[2];
/* Element =
   Description = Quantity */
	double quantity;
/* Element = DATE
   Description = Post date (YYYYMMDD) */
	char postdate[8];
/* Element = TIME
   Description = Post time (HHMMSS) */
	char posttime[6];
/* Element = BATCH
   Description = Batch number used for posting */
	char batch[6];
};
#pragma member_alignment restore
