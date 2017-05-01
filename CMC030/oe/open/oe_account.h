/*
 * File Layout for: OE.OE_ACCOUNT on 21-May-01
 *
 * Sales Order Account Table
 */

#pragma member_alignment save
#pragma nomember_alignment

struct oe_account_cdd
{
/* Element = CUSTTYPE
   Description = Customer type */
	char custtype[2];
/* Element =
   Description = Sales Order Type */
	char ordtype[2];
/* Element = LOCATION
   Description = Location number */
	char location[4];
/* Element = ACCOUNT
   Description = AR GL Account Number */
	char account[18];
/* Element = ACCOUNT
   Description = Order Discount GL Account Number */
	char disacct[18];
/* Element = ACCOUNT
   Description = Freight General Ledger Account Number */
	char fracct[18];
/* Element = ACCOUNT
   Description = Handling GL Account Number */
	char handling[18];
/* Element = ACCOUNT
   Description = Seles Account Number */
	char sales[18];
/* Element = ACCOUNT
   Description = COS Account Number */
	char cosacct[18];
};
#pragma member_alignment restore
