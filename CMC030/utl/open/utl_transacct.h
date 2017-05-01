/*
 * File Layout for: UTL.UTL_TRANSACCT on 21-May-01
 *
 * Inventory Transaction GL Accounts
 */

#pragma member_alignment save
#pragma nomember_alignment

struct utl_transacct_cdd
{
/* Element = LOCATION
   Description = Location number */
	char location[4];
/* Element = TRANSTYPE
   Description = Transaction type code */
	char transtype[2];
/* Element = PRODTYPE
   Description = Inventory Product Type */
	char prodtype[2];
/* Element = ACCOUNT
   Description = General Ledger Account Number */
	char account[18];
};
#pragma member_alignment restore
