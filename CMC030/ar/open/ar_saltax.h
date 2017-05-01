/*
 * File Layout for: AR.AR_SALTAX on 21-May-01
 *
 * Sales Tax Table
 */

#pragma member_alignment save
#pragma nomember_alignment

struct ar_saltax_cdd
{
/* Element = COUNTRY
   Description = Country */
	char country[2];
/* Element = STATE
   Description = State */
	char state[2];
/* Element = COUNTY
   Description = County */
	char county[2];
/* Element =
   Description = Sales Tax Percentage */
	double percent;
/* Element = ACCOUNT
   Description = General Ledger Account Number */
	char account[18];
};
#pragma member_alignment restore
