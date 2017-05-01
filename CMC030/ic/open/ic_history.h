/*
 * File Layout for: IC.IC_HISTORY on 21-May-01
 *
 * Product Balance History
 */

#pragma member_alignment save
#pragma nomember_alignment

struct ic_history_cdd
{
/* Element = PRODUCT
   Description = Product Number */
	char product[14];
/* Element = LOCATION
   Description = Location number */
	char location[4];
/* Element =
   Description = Period (YYYYPP) */
	char period[6];
/* Element =
   Description = Transaction type */
	char transtype[2];
/* Element =
   Description = Beginning Quantity */
	double bquantity;
/* Element =
   Description = Period Posted Quantity */
	double pquantity;
/* Element = DATE
   Description = Post date */
	char postdate[8];
/* Element = TIME
   Description = Post time */
	char posttime[6];
/* Element = BATCH
   Description = Batch number used for posting */
	char batch[6];
/* Element =
   Description = Amount of Sale */
	double saleamt;
/* Element =
   Description = Amount of Cost of Sale */
	double costamt;
};
#pragma member_alignment restore
