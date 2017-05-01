/*
 * File Layout for: AR.AR_CUSTOM on 21-May-01
 *
 * Accounts Receivable Customer File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct ar_custom_cdd
{
/* Element = CUSNUM
   Description = Customer Number */
	char cusnum[10];
/* Element =
   Description = Customer name */
	char cusnam[50];
/* Element =
   Description = Customer address 1 */
	char add1[25];
/* Element =
   Description = Customer address 2 */
	char add2[21];
/* Element =
   Description = Customer city */
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
/* Element =
   Description = Unused */
	char filler1[4];
/* Element = PHONE
   Description = Phone number */
	char phone[10];
/* Element =
   Description = O-open item, B-balance forward */
	char method[1];
/* Element =
   Description = Statement (y/n) */
	char stmtflg[1];
/* Element =
   Description = Alpha sort field */
	char alpsrt[15];
/* Element =
   Description = Service charge (y/n) */
	char serchrg[1];
/* Element =
   Description = Purge flag (Y or N) */
	char purge[1];
/* Element =
   Description = UNUSED SPACE */
	char filler[9];
};
#pragma member_alignment restore
