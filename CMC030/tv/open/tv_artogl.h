/*
 * File Layout for: TV.TV_ARTOGL on 21-May-01
 *
 * TV Ar to Gl Conversion Table
 */

#pragma member_alignment save
#pragma nomember_alignment

struct tv_artogl_cdd
{
/* Element =
   Description = Customer type */
	char custyp[2];
/* Element =
   Description = Description */
	char descr[20];
/* Element = ACCOUNT
   Description = General Ledger Account Number */
	char ar_acct[18];
/* Element = ACCOUNT
   Description = General Ledger Account Number */
	char sale_acct[18];
/* Element = ACCOUNT
   Description = General Ledger Account Number */
	char disc_acct[18];
/* Element = ACCOUNT
   Description = General Ledger Account Number */
	char production_acct[18];
/* Element =
   Description = Discount Percentage */
	int disc_per;
};
#pragma member_alignment restore
