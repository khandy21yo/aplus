/*
 * File Layout for: PP.PP_DAILY on 21-May-01
 *
 * Daily Transaction File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct pp_daily_cdd
{
/* Element = CUSTOMER
   Description = Customer Number */
	char cusnum[10];
/* Element = CARD
   Description = Pacific Pride Vehicle Card Number */
	char vehicle[8];
/* Element = CARD
   Description = Pacific Pride Driver Card Number */
	char driver[8];
/* Element = DATE
   Description = Transaction Date (YYYYMMDD) */
	char trandate[8];
/* Element = TIME
   Description = Transaction Time (HHMMSS) */
	char trantime[6];
/* Element =
   Description = Host Number */
	char host[4];
/* Element =
   Description = Site Number */
	char site[4];
/* Element =
   Description = Site Type */
	char stype[1];
/* Element = PRODUCT
   Description = Product Number */
	char product[14];
/* Element = UOM
   Description = Unit of measurement */
	char uom[2];
/* Element =
   Description = Quantity Sold */
	double quantity;
/* Element =
   Description = Odometer reading */
	double odom;
/* Element =
   Description = */
	char sltype[1];
/* Element =
   Description = */
	char ftype[1];
/* Element =
   Description = Selling Price */
	double sellprice;
/* Element =
   Description = Transaction Cost */
	double trancost;
/* Element =
   Description = Misc. keyboard entry */
	char misckeyb[9];
/* Element =
   Description = */
	char trntype[2];
/* Element =
   Description = Discount */
	char discount[4];
/* Element = DATE
   Description = icb Date (YYYYMMDD) */
	char icbdate[8];
/* Element =
   Description = Transaction number */
	char trnnum[5];
/* Element =
   Description = Sales Tax Rate */
	double staxrate;
/* Element =
   Description = Pump Number */
	char pump[2];
/* Element =
   Description = */
	char buyfran[4];
/* Element = DATE
   Description = Capture Date (YYYYMMDD) */
	char capdate[8];
/* Element = TIME
   Description = Capture Time (HHMMSS) */
	char captime[6];
/* Element =
   Description = */
	char postbnum[4];
/* Element =
   Description = */
	char transource[1];
/* Element =
   Description = */
	char editact[1];
/* Element =
   Description = */
	char julianday[3];
/* Element =
   Description = */
	char rstation[1];
/* Element = STATE
   Description = State */
	char state[2];
/* Element = BATCH
   Description = Batch number used for process (post,clos */
	char batch[6];
/* Element =
   Description = PP Cust Number */
	char identity[8];
};
#pragma member_alignment restore
