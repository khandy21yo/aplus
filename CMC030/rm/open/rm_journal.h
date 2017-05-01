/*
 * File Layout for: RM.RM_JOURNAL on 21-May-01
 *
 * Restaurant Journal
 */

#pragma member_alignment save
#pragma nomember_alignment

struct rm_journal_cdd
{
/* Element = LOCATION
   Description = Location number */
	char location[4];
/* Element = DATE
   Description = First date from worksheet */
	char startdate[8];
/* Element = TRANSTYPE
   Description = Transaction type code */
	char transtype[2];
/* Element = PRICETYPE
   Description = Price type */
	char pricetype[2];
/* Element =
   Description = Station man */
	char stationman[10];
/* Element = ACCOUNT
   Description = Expanse account number */
	char expaccount[18];
};
#pragma member_alignment restore
