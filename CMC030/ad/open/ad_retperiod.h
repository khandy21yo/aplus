/*
 * File Layout for: AD.AD_RETPERIOD on 21-May-01
 *
 * Retired Asset Last Period
 */

#pragma member_alignment save
#pragma nomember_alignment

struct ad_retperiod_cdd
{
/* Element = ASSET_NUM
   Description = Asset number */
	char asset_num[10];
/* Element = DEP_OBJECT
   Description = Depreciation object */
	char dep_object[1];
/* Element = PERIOD
   Description = Period (YYYYPP) */
	char period[6];
};
#pragma member_alignment restore
