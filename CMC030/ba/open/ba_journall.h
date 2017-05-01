/*
 * File Layout for: BA.BA_JOURNALL on 21-May-01
 *
 * Agency Journal
 */

#pragma member_alignment save
#pragma nomember_alignment

struct ba_journall_cdd
{
/* Element =
   Description = Billing Number */
	char billnum[10];
/* Element = EMPLOYEE
   Description = Client Number */
	char empnum[10];
/* Element =
   Description = Days worked */
	double days;
/* Element =
   Description = Hours Worked */
	double hours;
/* Element =
   Description = Units Worked */
	double units;
/* Element =
   Description = Wages Worked */
	double wages;
/* Element =
   Description = Rate billed */
	double rate;
/* Element =
   Description = Fee Billed */
	double fee;
/* Element = ACCOUNT
   Description = General Ledger Account Number */
	char acct[18];
/* Element =
   Description = Billing Method */
	char method[1];
};
#pragma member_alignment restore
