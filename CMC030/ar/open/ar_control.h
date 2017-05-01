/*
 * File Layout for: AR.AR_CONTROL on 21-May-01
 *
 * Accounts Receivable Control File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct ar_control_cdd
{
/* Element = ACCOUNT
   Description = AR account */
	char ar_acct[18];
/* Element =
   Description = Retention cycle */
	int retain;
/* Element =
   Description = Last Period Closed */
	int lastperclose;
/* Element =
   Description = Current Year */
	char year[4];
/* Element =
   Description = Closing flag 0-nostate,1-close,2-reset */
	char closeflag[1];
/* Element =
   Description = Number of days in a period */
	int ageper[5];
/* Element =
   Description = Names of periods */
	char agenam[5][16];
/* Element =
   Description = Customer Title (Cust, client, patient) */
	char ctitle[16];
/* Element =
   Description = Default Method for AR */
	char method[1];
};
#pragma member_alignment restore
