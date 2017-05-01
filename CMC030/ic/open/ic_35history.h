/*
 * File Layout for: IC.IC_35HISTORY on 21-May-01
 *
 * Inventory Transaction History File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct ic_35history_cdd
{
/* Element = PRODUCT
   Description = Product Number */
	char product[14];
/* Element = LOCATION
   Description = Location number */
	char location[4];
/* Element =
   Description = Transaction type */
	char transtype[2];
/* Element = CROSSREF
   Description = Cross Reference */
	char crossref[10];
/* Element =
   Description = Subaccount */
	char subacct[10];
/* Element =
   Description = Period Quantity */
	double pquantity[13];
/* Element =
   Description = Amount of Price */
	double priceamt[13];
/* Element =
   Description = Amount of Cost of Sale */
	double costamt[13];
};
#pragma member_alignment restore
