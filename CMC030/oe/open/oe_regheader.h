/*
 * File Layout for: OE.OE_REGHEADER on 21-May-01
 *
 * Sales Order Register Header File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct oe_regheader_cdd
{
/* Element = ORDNUM
   Description = Order Number */
	char ordnum[10];
/* Element = ORDTYPE
   Description = Sales Order Type */
	char ordtype[2];
/* Element =
   Description = Order Category */
	char ordcat[4];
/* Element =
   Description = Order Date */
	char orddate[8];
/* Element = ASTATUS
   Description = Activity status */
	char astatus[1];
/* Element =
   Description = Date */
	char sdate[8];
/* Element =
   Description = Customer Number */
	char cusnum[10];
/* Element =
   Description = Shipping Name */
	char shipnam[46];
/* Element = SHIPLIN
   Description = Shipping location number */
	char shiplin[4];
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
/* Element = DEPOSIT
   Description = Deposit number */
	char deposit[6];
/* Element =
   Description = Customer PO. (Obsolete remainder) */
	char oldcustpo[4];
/* Element =
   Description = Ship Via */
	char shipvia[2];
/* Element =
   Description = Terms */
	char terms[2];
/* Element =
   Description = Discount Percentage */
	double disc;
/* Element = TAXCODE
   Description = Tax code */
	char taxcode[2];
/* Element = TAXFLAG
   Description = Tax Flag */
	char taxflag[1];
/* Element = LOCATION
   Description = Location number */
	char location[4];
/* Element =
   Description = Commission amount */
	double commamt;
/* Element =
   Description = Salesmen */
	char salesman[10];
/* Element = OPERATOR
   Description = Operator */
	char operator[10];
/* Element =
   Description = Commission for salesmen */
	double salcomm;
/* Element =
   Description = Amount Paid */
	double amtpaid;
/* Element =
   Description = Packing List Release Number */
	char shipno[2];
/* Element = BATCH
   Description = Batch number used for process (post,clos */
	char batch[6];
/* Element = NOTES
   Description = Notes */
	char notes[3][40];
/* Element =
   Description = Purchase order number */
	char custpo[20];
};
#pragma member_alignment restore
