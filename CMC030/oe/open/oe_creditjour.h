/*
 * File Layout for: OE.OE_CREDITJOUR on 21-May-01
 *
 * Credit Memo Journal Header
 */

#pragma member_alignment save
#pragma nomember_alignment

struct oe_creditjour_cdd
{
/* Element =
   Description = Memo Number */
	char memonum[8];
/* Element =
   Description = Memo Date */
	char memodate[8];
/* Element =
   Description = Order Date */
	char orddate[8];
/* Element =
   Description = Handling */
	double handling;
/* Element =
   Description = Order Discount Amount */
	double disc;
/* Element =
   Description = Miscellaneous Charges */
	double misc;
/* Element =
   Description = Miscellaneous Charges GL Account */
	char miscacct[18];
/* Element =
   Description = Freight Amount */
	double freight;
/* Element =
   Description = Sales Tax Amount */
	double salestax;
/* Element =
   Description = Operator */
	char operator[10];
/* Element =
   Description = Notes */
	char notes[2][40];
/* Element =
   Description = Reason Code */
	char reason[2];
/* Element = CUSTOMER
   Description = Customer Number */
	char cusnum[10];
/* Element = LOCATION
   Description = Location number */
	char location[4];
/* Element = ORDTYPE
   Description = Order type */
	char ordtype[2];
/* Element = SALESMAN
   Description = Salesperson number */
	char salesman[10];
/* Element =
   Description = Shipping Name */
	char shipnam[50];
/* Element =
   Description = Address, line 1 */
	char add1[25];
/* Element =
   Description = Address, line 2 */
	char add2[25];
/* Element =
   Description = Address, line 3 */
	char add3[25];
/* Element =
   Description = City */
	char city[15];
/* Element =
   Description = State */
	char state[2];
/* Element =
   Description = Zip Code */
	char zip[10];
/* Element =
   Description = Country */
	char country[2];
};
#pragma member_alignment restore
