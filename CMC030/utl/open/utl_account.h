/*
 * File Layout for: UTL.UTL_ACCOUNT on 21-May-01
 *
 * Transaction Account Table
 */

#pragma member_alignment save
#pragma nomember_alignment

struct utl_account_cdd
{
/* Element = LOCATION
   Description = Location number */
	char location[4];
/* Element = TRANSTYPE
   Description = Transaction type code */
	char transtype[2];
/* Element = ACCOUNT
   Description = General Ledger Account Number */
	char account[18];
};
#pragma member_alignment restore
