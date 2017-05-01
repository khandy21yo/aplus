/*
 * File Layout for: AP.AP_CONTROL on 21-May-01
 *
 * Accounts Payable Control File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct ap_control_cdd
{
/* Element =
   Description = */
	char ap_acct[18];
/* Element =
   Description = */
	char disclost_acct[18];
/* Element =
   Description = */
	char cash_acct[18];
/* Element =
   Description = */
	char last_trankey[6];
/* Element =
   Description = */
	char last_cknum[6];
/* Element =
   Description = Retention cycle */
	int retain;
/* Element =
   Description = Last period closed */
	int lastperclose;
/* Element =
   Description = Retain 1099 history only (Y/N) */
	char retain_1099_only[1];
/* Element =
   Description = Year */
	char year[4];
/* Element =
   Description = Closing flag 0-nostate,1-close,2-reset */
	char closeflag[1];
};
#pragma member_alignment restore
