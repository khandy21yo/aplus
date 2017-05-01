/*
 * File Layout for: MO.MO_ORDERJOUR on 21-May-01
 *
 * Manufacturing Order Journal
 */

#pragma member_alignment save
#pragma nomember_alignment

struct mo_orderjour_cdd
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
   Description = Ship Name */
	char shipnam[50];
/* Element =
   Description = Ship To Address 1 */
	char add1[25];
/* Element =
   Description = Ship To Address 2 */
	char add2[25];
/* Element =
   Description = Ship To Address 3 */
	char add3[25];
/* Element =
   Description = Ship To City */
	char city[15];
/* Element =
   Description = Ship To State */
	char state[2];
/* Element =
   Description = Ship To Zip Code */
	char zip[10];
/* Element =
   Description = Ship To Country */
	char country[2];
/* Element =
   Description = Customer PO. */
	char custpo[10];
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
	char salesman[2][10];
/* Element =
   Description = Commission for salesmen */
	double salcomm[2];
/* Element =
   Description = Paid Amount */
	double amtpaid;
/* Element = CHECK
   Description = Check number */
	char check[6];
/* Element =
   Description = Notes */
	char notes[4][40];
/* Element =
   Description = Freight Number */
	double freight;
/* Element = TAXCODE
   Description = Tax code */
	char taxcode[2];
/* Element = TAXFLAG
   Description = Tax Flag */
	char taxflag[1];
/* Element =
   Description = Line Number of Ship To Code */
	char shiplin[4];
};
#pragma member_alignment restore
