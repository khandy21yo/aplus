/*
 * File Layout for: UTL.UTL_LOCATION on 21-May-01
 *
 * Location Profile
 */

#pragma member_alignment save
#pragma nomember_alignment

struct utl_location_cdd
{
/* Element = LOCATION
   Description = Location number */
	char location[4];
/* Element =
   Description = Location name */
	char locname[40];
/* Element = REGION
   Description = Region number */
	char region[2];
/* Element = LOCGROUP
   Description = Location group number */
	char locgroup[2];
/* Element = STREET
   Description = Address (street) */
	char address1[25];
/* Element = POBOX
   Description = Address (P.O.Box) */
	char address2[21];
/* Element = CITY
   Description = City */
	char city[15];
/* Element = STATE
   Description = State */
	char state[2];
/* Element = ZIP
   Description = Zip code */
	char zip[10];
/* Element = COUNTY
   Description = County */
	char county[2];
/* Element = COUNTRY
   Description = Country */
	char country[2];
/* Element = PHONE
   Description = Phone number */
	char phone[10];
/* Element = ADDRESS1
   Description = Ship to address, line 1 */
	char shpaddress1[25];
/* Element = ADDRESS2
   Description = Ship to address, line 2 */
	char shpaddress2[21];
/* Element = CITY
   Description = Ship to City */
	char shpcity[15];
/* Element = STATE
   Description = Ship to State */
	char shpstate[2];
/* Element = ZIP
   Description = Ship to Zip Code */
	char shpzip[10];
/* Element = COUNTY
   Description = Ship to County */
	char shpcounty[2];
/* Element = COUNTRY
   Description = Ship to Country */
	char shpcountry[2];
/* Element = PHONE
   Description = Ship to Phone number */
	char shpphone[10];
};
#pragma member_alignment restore
