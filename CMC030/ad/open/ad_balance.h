/*
 * File Layout for: AD.AD_BALANCE on 21-May-01
 *
 * Asset Depreciation Balances
 */

#pragma member_alignment save
#pragma nomember_alignment

struct ad_balance_cdd
{
/* Element = ASSET_NUM
   Description = Asset number */
	char asset_num[10];
/* Element = DEP_OBJECT
   Description = Depreciation object */
	char dep_object[1];
/* Element =
   Description = Depreciation status (active,retired..) */
	char dep_status[1];
/* Element =
   Description = Total depreciated dollars */
	double amount_ctd;
/* Element =
   Description = Total units */
	double unit_ctd;
/* Element = PERIOD
   Description = Last period updated */
	char lastper[6];
};
#pragma member_alignment restore
