/*
 * File Layout for: MO.MO_REGHEADER on 21-May-01
 *
 * MO Register Header
 */

#pragma member_alignment save
#pragma nomember_alignment

struct mo_regheader_cdd
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
/* Element =
   Description = Customer PO. */
	char custpo[10];
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
	char salesman[2][10];
/* Element =
   Description = Commission for salesmen */
	double salcomm[2];
/* Element =
   Description = Packing List Release Number */
	char shipno[2];
/* Element = BATCH
   Description = Batch number used for process (post,clos */
	char batch[6];
};
#pragma member_alignment restore
