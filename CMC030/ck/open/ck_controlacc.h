/*
 * File Layout for: CK.CK_CONTROLACC on 21-May-01
 *
 * Check Account Control File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct ck_controlacc_cdd
{
/* Element = ACCOUNT
   Description = General Ledger Account Number */
	char account[18];
/* Element =
   Description = Bank Account */
	char bank_acct[6];
/* Element =
   Description = Start Check Number */
	char startck[6];
/* Element =
   Description = End Check Number */
	char endck[6];
/* Element =
   Description = Bank Number */
	char bank_num[20];
};
#pragma member_alignment restore
