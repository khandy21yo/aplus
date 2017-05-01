/*
 * File Layout for: BS.BS_CLIENT on 21-May-01
 *
 * Client File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct bs_client_cdd
{
/* Element = CLIENT
   Description = Client Number */
	char client[10];
/* Element =
   Description = Client name */
	char clientname[50];
/* Element =
   Description = Alpha sort */
	char alpsrt[15];
/* Element =
   Description = Client address 1 */
	char add1[25];
/* Element =
   Description = Client address 2 */
	char add2[21];
/* Element =
   Description = Client city */
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
   Description = */
	char refno[16];
/* Element =
   Description = */
	char birthdate[8];
/* Element =
   Description = */
	char sex[1];
/* Element =
   Description = */
	char onsetdate[8];
/* Element =
   Description = */
	char sstatus[1];
/* Element =
   Description = */
	char termdate[8];
};
#pragma member_alignment restore
