/*
 * File Layout for: AD.AD_HISTORY on 21-May-01
 *
 * Asset Depreciation History
 */

#pragma member_alignment save
#pragma nomember_alignment

struct ad_history_cdd
{
/* Element = ASSET_NUM
   Description = Asset number */
	char asset_num[10];
/* Element = DEP_OBJECT
   Description = Depreciation object */
	char dep_object[1];
/* Element = DEP_STATUS
   Description = Prior the asset activity status */
	char dep_status[1];
/* Element =
   Description = Depreciation amount */
	double amount_his;
/* Element =
   Description = Depreciation units */
	double unit_his;
/* Element = PERIOD
   Description = Fiscal year (YYYY) and Cycle (PP) */
	char period[6];
};
#pragma member_alignment restore
