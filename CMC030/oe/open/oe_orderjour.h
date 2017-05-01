/*
 * File Layout for: OE.OE_ORDERJOUR on 21-May-01
 *
 * Sales Order and Ticket Journal
 */

#pragma member_alignment save
#pragma nomember_alignment

struct oe_orderjour_cdd
{
/* Element =
   Description = Order Number */
	char ordnum[10];
/* Element =
   Description = Order Date */
	char orddate[8];
/* Element =
   Description = Order Type */
	char ordtype[2];
/* Element =
   Description = Order Category */
	char ordcat[4];
/* Element =
   Description = Customer Number */
	char cusnum[10];
/* Element =
   Description = Order Discount */
	double disc;
/* Element =
   Description = Miscellaneous Charges */
	double misc;
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
/* Element = DEPOSIT
   Description = Deposit number */
	char deposit[6];
/* Element =
   Description = Customer PO. (Obsolete remainder) */
	char oldcustpo[4];
/* Element =
   Description = Date */
	char shipdate[8];
/* Element =
   Description = Ship Via */
	char shipvia[2];
/* Element =
   Description = Terms */
	char terms[2];
/* Element =
   Description = Taxes */
	double salestax;
/* Element = LOCATION
   Description = Location number */
	char location[4];
/* Element =
   Description = Operator */
	char operator[10];
/* Element =
   Description = Commission amount */
	double commamt;
/* Element =
   Description = Commission percentage */
	double commperc;
/* Element =
   Description = Salesmen */
	char salesman[10];
/* Element =
   Description = Misc Charges Reason Code */
	char creason[2];
/* Element =
   Description = Commission for salesmen */
	double salcomm;
/* Element =
   Description = Handling Amount */
	double handling;
/* Element =
   Description = Paid Amount */
	double amtpaid;
/* Element = CHECK
   Description = Check number */
	char check[6];
/* Element =
   Description = Notes */
	char notes[3][40];
/* Element =
   Description = Miscellaneous Account */
	char miscacct[18];
/* Element = DATE
   Description = Transaction Date (YYYYMMDD) */
	char trandate[8];
/* Element = TIME
   Description = Transaction Time (HHMMSS) */
	char trantime[6];
/* Element =
   Description = Invoice Number */
	char invnum[8];
/* Element =
   Description = Freight Number */
	double freight;
/* Element = TAXCODE
   Description = Tax code */
	char taxcode[2];
/* Element = TAXFLAG
   Description = Tax Flag */
	char taxflag[1];
/* Element = LINE
   Description = Shipping Line Address Code */
	char shiplin[4];
/* Element =
   Description = Number of payments */
	int paymnt;
/* Element = FLAG
   Description = Register Flag - Order Exists in Register */
	char reg_flag[1];
/* Element =
   Description = Purchase order number */
	char custpo[20];
};
#pragma member_alignment restore
