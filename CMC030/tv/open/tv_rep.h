/*
 * File Layout for: TV.TV_REP on 21-May-01
 *
 * TV Rep Table
 */

#pragma member_alignment save
#pragma nomember_alignment

struct tv_rep_cdd
{
/* Element = TV_REP_NUM
   Description = Rep number */
	char rep_num[10];
/* Element = NAME
   Description = Name */
	char rname[25];
/* Element = ADD1
   Description = Address line 1 */
	char add1[25];
/* Element = ADD2
   Description = Address line 2 */
	char add2[21];
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
	char country[8];
/* Element = PHONE
   Description = Phone number */
	char phone[10];
/* Element = ALPSRT
   Description = Alpha sort key */
	char alpsrt[15];
/* Element =
   Description = Commission percent */
	double comm;
};
#pragma member_alignment restore
