/*
 * File Layout for: AR.AR_SERCHG on 21-May-01
 *
 * Service Charge Definition
 */

#pragma member_alignment save
#pragma nomember_alignment

struct ar_serchg_cdd
{
/* Element =
   Description = Country */
	char country[2];
/* Element =
   Description = State */
	char state[2];
/* Element = ACCOUNT
   Description = General Ledger Account Number */
	char acct[18];
/* Element = ACCOUNT
   Description = General Ledger Account Number */
	char screv[18];
/* Element =
   Description = Service charge percentage */
	double serchg;
/* Element =
   Description = Minimum chargeable */
	double minimum;
/* Element =
   Description = Dollar amount charge */
	double dollar;
/* Element =
   Description = Start Period for Service Charge */
	int speriod;
};
#pragma member_alignment restore
