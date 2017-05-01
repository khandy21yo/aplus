/*
 * File Layout for: BC.BC_CUSTYP on 21-May-01
 *
 * Billing to Customer Type Definitions
 */

#pragma member_alignment save
#pragma nomember_alignment

struct bc_custyp_cdd
{
/* Element =
   Description = Customer type */
	char custyp[2];
/* Element = ACCOUNT
   Description = General Ledger Account Number */
	char ar_acct[18];
/* Element =
   Description = Description */
	char descr[30];
};
#pragma member_alignment restore
