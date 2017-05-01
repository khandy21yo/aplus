/*
 * File Layout for: PD.PD_ACCOUNT on 21-May-01
 *
 * Product Type Account Table
 */

#pragma member_alignment save
#pragma nomember_alignment

struct pd_account_cdd
{
/* Element = PRODTYPE
   Description = Inventory Product Type */
	char prodtype[2];
/* Element = LOCATION
   Description = Location */
	char location[4];
/* Element = ACCOUNT
   Description = Inventory General Ledger Account Number */
	char invacct[18];
/* Element = ACCOUNT
   Description = Work in Process Account */
	char wipacct[18];
/* Element = ACCOUNT
   Description = COS General Ledger Account Number */
	char cosacct[18];
/* Element = ACCOUNT
   Description = Inv Disc General Ledger Account Number */
	char discacct[18];
/* Element = ACCOUNT
   Description = Miscellaneous Charges Account Num */
	char mischacct[18];
/* Element = ACCOUNT
   Description = Product Price Variance Account */
	char pricevaracct[18];
/* Element = ACCOUNT
   Description = Miscellaneous (2) Changes Account */
	char misch2acct[18];
};
#pragma member_alignment restore
