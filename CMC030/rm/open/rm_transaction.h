/*
 * File Layout for: RM.RM_TRANSACTION on 21-May-01
 *
 * Restaurant Transaction File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct rm_transaction_cdd
{
/* Element = PRODUCT
   Description = Product Number */
	char product[14];
/* Element = LOCATION
   Description = Location number */
	char location[4];
/* Element = DATE
   Description = Date (YYYYMMDD) */
	char transdate[8];
/* Element = TRANSTYPE
   Description = Transaction type code */
	char transtype[2];
/* Element =
   Description = Quantity */
	double quantity;
/* Element =
   Description = Price */
	double price;
/* Element =
   Description = Cost */
	double cost;
/* Element = STATIONMAN
   Description = Station man (operator) */
	char stationman[10];
/* Element = DATE
   Description = Post date */
	char postdate[8];
/* Element = TIME
   Description = Post time */
	char posttime[6];
/* Element = BATCH
   Description = Batch number used for posting */
	char batch[6];
};
#pragma member_alignment restore
