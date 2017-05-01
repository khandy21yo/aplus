/*
 * File Layout for: AR.AR_35CUSTOM on 21-May-01
 *
 * Customer Address File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct ar_35custom_cdd
{
/* Element = CUSNUM
   Description = Customer Number */
	char cusnum[10];
/* Element = COMPNAME
   Description = Person or Company Name */
	char cusnam[50];
/* Element = CUSTYPE
   Description = Customer type */
	char ttype[2];
/* Element = CATEGORY
   Description = Category */
	char category[4];
/* Element = DATE
   Description = Onset Date (YYYYMMDD) */
	char bdate[8];
/* Element = ASTATUS
   Description = Activity status */
	char sstatus[1];
/* Element = DATE
   Description = Ending Date (YYYYMMDD) */
	char edate[8];
/* Element = ADDRESS
   Description = Address line 1 */
	char add1[25];
/* Element = ADDRESS
   Description = Address line 2 */
	char add2[25];
/* Element = ADDRESS
   Description = Address line 3 */
	char add3[25];
/* Element = CITY
   Description = City */
	char city[15];
/* Element = STATE
   Description = State */
	char state[2];
/* Element = ZIP
   Description = Zip code */
	char zip[10];
/* Element = COUNTRY
   Description = Country */
	char country[2];
/* Element = COUNTY
   Description = County */
	char county[2];
/* Element = PHONE
   Description = Phone number */
	char phone[10];
/* Element =
   Description = O-open item, B-balance forward */
	char method[1];
/* Element =
   Description = Statement (Y/N) */
	char stmtflg[1];
/* Element = ALFASORT
   Description = Alpha Sort Key */
	char alpsrt[15];
/* Element = YESNO
   Description = Service Charge (Yes or No Flag) */
	char serchrg[1];
/* Element = TAXCODE
   Description = Tax code */
	char taxcode[2];
/* Element = TAXEXEMPT
   Description = Tax Exampt Permit Number */
	char taxexemp[15];
/* Element = LOCATION
   Description = Primary Location */
	char location[4];
/* Element = TERMS
   Description = Terms */
	char terms[2];
/* Element = CARRIER
   Description = Carrier Code (Ship Via) */
	char carrier[2];
/* Element = SALESMAN
   Description = Salesperson number */
	char salesman[10];
/* Element =
   Description = Credit Limit */
	double creditlim;
/* Element = DISCOUNT
   Description = Discount percentage */
	double discount;
/* Element = YESNO
   Description = Accept Backorders Flag (Y/N) */
	char backorder[1];
/* Element = TAXFLAG
   Description = Tax Flag */
	char taxflag[1];
};
#pragma member_alignment restore
