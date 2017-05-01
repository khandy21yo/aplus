/*
 * File Layout for: PS.PS_CASHREG on 21-May-01
 *
 * Cash Register Description Table
 */

#pragma member_alignment save
#pragma nomember_alignment

struct ps_cashreg_cdd
{
/* Element = CASHREG
   Description = Cash Register Number */
	char cashreg[4];
/* Element =
   Description = Description */
	char descr[20];
/* Element = LOCATION
   Description = Location number */
	char location[4];
/* Element = NOTES
   Description = Notes */
	char notes[2][40];
/* Element =
   Description = Last Invoice Number */
	char last_invnum[8];
/* Element = ACCOUNT
   Description = Petty Cash Account Number */
	char pettycash[18];
};
#pragma member_alignment restore
