/*
 * File Layout for: CK.CK_CKMNT on 21-May-01
 *
 * Check Maintainance
 */

#pragma member_alignment save
#pragma nomember_alignment

struct ck_ckmnt_cdd
{
/* Element =
   Description = Bank Account */
	char bank_acct[6];
/* Element = CHECK
   Description = Check number */
	char cknum[6];
/* Element =
   Description = Entry Flag */
	char etype[1];
/* Element =
   Description = From GL (G), or Bank (B) */
	char stype[1];
/* Element = DATE
   Description = Check Date */
	char ckdat[8];
/* Element =
   Description = Check Amount */
	double ckamt;
/* Element = PERIOD
   Description = GL Fiscal year (YYYY) and Cycle (PP) */
	char gldate[6];
};
#pragma member_alignment restore
