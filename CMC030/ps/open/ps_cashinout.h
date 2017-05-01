/*
 * File Layout for: PS.PS_CASHINOUT on 21-May-01
 *
 * Cash in and out of the cash Register
 */

#pragma member_alignment save
#pragma nomember_alignment

struct ps_cashinout_cdd
{
/* Element = DATE
   Description = Date (YYYYMMDD) */
	char cashdate[8];
/* Element = TIME
   Description = Time (HHMMSS) */
	char cashtime[6];
/* Element =
   Description = Cash Amount */
	double amount;
/* Element = NOTES
   Description = Notes */
	char notes[40];
/* Element = OPERATOR
   Description = Written By */
	char operator[10];
/* Element = DEPOSIT
   Description = Deposit number */
	char deposit[6];
/* Element = ACCOUNT
   Description = General Ledger Account Number */
	char account[18];
};
#pragma member_alignment restore
