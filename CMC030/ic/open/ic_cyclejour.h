/*
 * File Layout for: IC.IC_CYCLEJOUR on 21-May-01
 *
 * Cycle Counting Journal
 */

#pragma member_alignment save
#pragma nomember_alignment

struct ic_cyclejour_cdd
{
/* Element = LOCATION
   Description = Location number */
	char location[4];
/* Element = DATE
   Description = Counting Date (YYYYMMDD) */
	char countdate[8];
/* Element = TRANSTYPE
   Description = Transaction type code */
	char transtype[2];
/* Element = PRIMREF
   Description = Primary reference */
	char primref[8];
/* Element = SECREF
   Description = Secondary reference */
	char secref[8];
/* Element = CROSSREF
   Description = Cross reference number */
	char crossref[10];
/* Element = SUBACCT
   Description = Sub account (job number) */
	char subacct[10];
/* Element = STATIONMAN
   Description = Station man (operator) */
	char stationman[10];
};
#pragma member_alignment restore
