/*
 * File Layout for: PW.PW_JH on 21-May-01
 *
 * PW Journal Header
 */

#pragma member_alignment save
#pragma nomember_alignment

struct pw_jh_cdd
{
/* Element = ORDNUM
   Description = Order Number */
	char ordnum[10];
/* Element = CUSTOMER
   Description = Customer Sold To */
	char soldto[10];
/* Element = CUSTOMER
   Description = Customer Shipped To */
	char shipto[10];
/* Element = INVOICE
   Description = Invoice number */
	char invnum[8];
/* Element = DATE
   Description = Invoice Date (YYYYMMDD) */
	char invdat[8];
/* Element = DATE
   Description = Ship Date (YYYYMMDD) */
	char shpdat[8];
/* Element = PO
   Description = Purchase order number */
	char cuspo[10];
/* Element =
   Description = Sold By */
	char soldby[10];
/* Element = TERMS
   Description = Terms */
	char terms[2];
/* Element = CARRIER
   Description = Carrier Code (Ship Via) */
	char carnam[2];
/* Element = FOB
   Description = F.O.B. */
	char fobflg[1];
/* Element =
   Description = Line Count */
	long line1;
/* Element =
   Description = Line Count */
	long line2;
};
#pragma member_alignment restore
