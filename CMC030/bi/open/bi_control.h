/*
 * File Layout for: BI.BI_CONTROL on 21-May-01
 *
 * Billing to Insurance Control File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct bi_control_cdd
{
/* Element = INVOICE
   Description = Invoice number */
	char invoice[8];
/* Element = ACCOUNT
   Description = AR General Ledger Account Number */
	char account[18];
};
#pragma member_alignment restore
