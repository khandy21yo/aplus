/*
 * File Layout for: PW.PW_JL on 21-May-01
 *
 * PW Journal Line
 */

#pragma member_alignment save
#pragma nomember_alignment

struct pw_jl_cdd
{
/* Element = ORDNUM
   Description = Order Number */
	char ordnum[10];
/* Element =
   Description = Window (1,2) */
	char window[1];
/* Element = LINE
   Description = Line */
	char jline[4];
/* Element = PRODUCT
   Description = Product Number */
	char pronum[14];
/* Element = LOCATION
   Description = Store number */
	char stonum[4];
/* Element = LOT
   Description = Lot Number */
	char lotnum[10];
/* Element = DESCRIPTION
   Description = Description */
	char description[40];
/* Element = ACCOUNT
   Description = General Ledger Account Number */
	char accnum[18];
/* Element = QUANTITY
   Description = Quantity */
	double qty;
/* Element =
   Description = Price */
	double price;
/* Element =
   Description = Weight */
	double pounds;
/* Element =
   Description = Extension */
	double ext;
/* Element = PCTYPE
   Description = Price Flag */
	char prtype[2];
/* Element = VENDOR
   Description = Vendor Number */
	char vennum[10];
};
#pragma member_alignment restore
