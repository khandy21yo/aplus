/*
 * File Layout for: IC.IC_35BALANCE on 21-May-01
 *
 * Inventory Product Balance File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct ic_35balance_cdd
{
/* Element = PRODUCT
   Description = Product Number */
	char product[14];
/* Element = LOCATION
   Description = Location number */
	char location[4];
/* Element = TRANSTYPE
   Description = Transaction type code */
	char transtype[2];
/* Element =
   Description = Beginning Balance */
	double bbalance;
/* Element =
   Description = Posted Quantity */
	double pbalance;
/* Element =
   Description = Running Balance (journals) */
	double rbalance;
/* Element =
   Description = Control Balance */
	double cbalance;
};
#pragma member_alignment restore
