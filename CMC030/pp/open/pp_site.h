/*
 * File Layout for: PP.PP_SITE on 21-May-01
 *
 * Pacific Pride Site File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct pp_site_cdd
{
/* Element =
   Description = Host # */
	char host[4];
/* Element =
   Description = Site Code */
	char site[4];
/* Element =
   Description = site Type */
	char stype[1];
/* Element = NAME
   Description = Site Name */
	char sname[30];
/* Element = ADDRESS
   Description = Address Line */
	char address[25];
/* Element = CITY
   Description = City */
	char city[15];
/* Element = STATE
   Description = State */
	char state[2];
/* Element = ZIP
   Description = Zip code */
	char zip[10];
/* Element =
   Description = Local Sale Location */
	char locsale[3];
/* Element =
   Description = Foreign Sale Location */
	char forsale[3];
/* Element =
   Description = Foreign Purchase Location */
	char forpur[3];
};
#pragma member_alignment restore
