/*
 * File Layout for: SB.SB_ACCOUNT on 21-May-01
 *
 * GL Accounts for Subsidiary Systems
 */

#pragma member_alignment save
#pragma nomember_alignment

struct sb_account_cdd
{
/* Element = SYSTEM
   Description = Software System code */
	char system[2];
/* Element = ACCOUNT
   Description = General Ledger Account Number */
	char account[18];
/* Element =
   Description = Account Group */
	char acctgroup[4];
};
#pragma member_alignment restore
