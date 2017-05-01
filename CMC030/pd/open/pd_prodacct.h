/*
 * File Layout for: PD.PD_PRODACCT on 21-May-01
 *
 * Product GL Account Table
 */

#pragma member_alignment save
#pragma nomember_alignment

struct pd_prodacct_cdd
{
/* Element = LOCATION
   Description = Location number */
	char location[4];
/* Element = PRODTYPE
   Description = Inventory Product Type */
	char prodtype[2];
/* Element = ACCOUNT
   Description = Inventory GL Account Number */
	char invacc[18];
/* Element = ACCOUNT
   Description = Cost of Sale GL Account Number */
	char cosacct[18];
/* Element = ACCOUNT
   Description = Product Discount GL Account Number */
	char discacct[18];
};
#pragma member_alignment restore
