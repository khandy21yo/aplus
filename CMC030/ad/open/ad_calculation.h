/*
 * File Layout for: AD.AD_CALCULATION on 21-May-01
 *
 * Asset Depreciation Ledger
 */

#pragma member_alignment save
#pragma nomember_alignment

struct ad_calculation_cdd
{
/* Element = ASSET_NUM
   Description = Asset number */
	char asset_num[10];
/* Element = DEP_OBJECT
   Description = Depreciation object */
	char dep_object[1];
/* Element =
   Description = Depreciation status (active,retired) */
	char dep_status[1];
/* Element =
   Description = Current depreciated dollars */
	double amount_cur;
/* Element =
   Description = Current units */
	double unit_cur;
};
#pragma member_alignment restore
