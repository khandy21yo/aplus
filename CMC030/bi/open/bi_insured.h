/*
 * File Layout for: BI.BI_INSURED on 21-May-01
 *
 * Insured File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct bi_insured_cdd
{
/* Element = CUSNUM
   Description = Insured Number */
	char insured[10];
/* Element =
   Description = Insured name */
	char insname[50];
/* Element =
   Description = Insured address, line 1 */
	char add1[25];
/* Element =
   Description = Insured address, line 2 */
	char add2[21];
/* Element =
   Description = Insured city */
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
   Description = Alpha sort field */
	char alpsrt[15];
/* Element = REFNO
   Description = Reference number */
	char refno[16];
/* Element = DATE
   Description = Birthdate (YYYYMMDD) */
	char birthdate[8];
/* Element = SEX
   Description = Sex */
	char sex[1];
/* Element = DATE
   Description = Onset date (YYYYMMDD) */
	char onsetdate[8];
};
#pragma member_alignment restore
