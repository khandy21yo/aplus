/*
 * File Layout for: OE.OE_SHIPTO on 21-May-01
 *
 * Ship to Address
 */

#pragma member_alignment save
#pragma nomember_alignment

struct oe_shipto_cdd
{
/* Element =
   Description = Customer Number */
	char cusnum[10];
/* Element = LINE
   Description = Line */
	char lines[4];
/* Element =
   Description = Shipping Name */
	char shipnam[50];
/* Element =
   Description = Address 1 */
	char add1[25];
/* Element =
   Description = Address 2 */
	char add2[25];
/* Element =
   Description = Address 3 */
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
   Description = Ship Via */
	char shipvia[2];
/* Element = LOCATION
   Description = Location number */
	char location[4];
/* Element =
   Description = Salesmen */
	char salesman[10];
/* Element = TAXCODE
   Description = Tax code */
	char taxcode[2];
/* Element = TAXEXEMPT
   Description = Tax Exampt Permit Number */
	char taxexemp[15];
/* Element =
   Description = Ship to adrress notes */
	char notes[3][40];
/* Element = PHONE
   Description = Phone number */
	char phone[10];
};
#pragma member_alignment restore
