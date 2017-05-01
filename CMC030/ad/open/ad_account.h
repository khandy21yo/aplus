/*
 * File Layout for: AD.AD_ACCOUNT on 21-May-01
 *
 * Asset and Depreciation Account Table
 */

#pragma member_alignment save
#pragma nomember_alignment

struct ad_account_cdd
{
/* Element = LOCATION
   Description = Location number */
	char location[4];
/* Element =
   Description = Asset type */
	char asset_type[2];
/* Element = ACCOUNT
   Description = Asset Account Number */
	char ass_acct[18];
/* Element = ACCOUNT
   Description = Depreciation Account Number */
	char dep_acct[18];
/* Element = ACCOUNT
   Description = Expense Account Number */
	char exp_acct[18];
};
#pragma member_alignment restore
