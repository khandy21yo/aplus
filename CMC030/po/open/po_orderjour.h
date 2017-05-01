/*
 * File Layout for: PO.PO_ORDERJOUR on 21-May-01
 *
 * Purchase Order Header
 */

#pragma member_alignment save
#pragma nomember_alignment

struct po_orderjour_cdd
{
/* Element = PO
   Description = Purchase order number */
	char po[10];
/* Element =
   Description = Purchase Order Type */
	char potype[2];
/* Element = DATE
   Description = Release Date (YYYYMMDD) */
	char podate[8];
/* Element = VENDOR
   Description = Vendor Number (Seller) */
	char vendor[10];
/* Element =
   Description = Buyer */
	char buyer[10];
/* Element =
   Description = Salesman */
	char salesman[10];
/* Element = TERM
   Description = Terms */
	char terms[2];
/* Element =
   Description = Carrier */
	char carrier[2];
/* Element =
   Description = FOB Code */
	char fob[2];
/* Element =
   Description = Acknowledgement code */
	char acknow[2];
/* Element =
   Description = Collect / Pre-paid */
	char col_ppd[1];
/* Element =
   Description = Notes */
	char note[4][40];
/* Element =
   Description = Person who typed in data */
	char operator[10];
/* Element =
   Description = Print form (Yes/No) */
	char printform[1];
/* Element = LOCATION
   Description = Location number */
	char fromlocation[4];
/* Element = NAME
   Description = Name */
	char toname[30];
/* Element =
   Description = Address 1 */
	char toadd1[25];
/* Element =
   Description = Address 2 */
	char toadd2[25];
/* Element = CITY
   Description = City */
	char tocity[15];
/* Element = STATE
   Description = State */
	char tostate[2];
/* Element = ZIP
   Description = Zip code */
	char tozip[10];
/* Element = COUNTRY
   Description = Country */
	char tocountry[2];
/* Element =
   Description = Batch Number */
	char batch[2];
/* Element = FLAG
   Description = Po Flag if this is a new PO */
	char new[1];
};
#pragma member_alignment restore
