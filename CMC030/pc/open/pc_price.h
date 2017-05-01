/*
 * File Layout for: PC.PC_PRICE on 21-May-01
 *
 * Product Price
 */

#pragma member_alignment save
#pragma nomember_alignment

struct pc_price_cdd
{
/* Element = PRODUCT_NUM
   Description = Product number */
	char product_num[14];
/* Element = LOCATION
   Description = Location number */
	char location[4];
/* Element = PCTYPE
   Description = Price cost type */
	char pctype[2];
/* Element = DATE
   Description = Date (MMDDYYYY) */
	char xdate[8];
/* Element = TIME
   Description = Time (HHMMSS) */
	char xtime[6];
/* Element = AMOUNT
   Description = Dollar amount. */
	double pricecost;
};
#pragma member_alignment restore
