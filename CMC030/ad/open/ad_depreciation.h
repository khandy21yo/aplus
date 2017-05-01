/*
 * File Layout for: AD.AD_DEPRECIATION on 21-May-01
 *
 * Asset Depreciation File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct ad_depreciation_cdd
{
/* Element = ASSET_NUM
   Description = Asset number */
	char asset_num[10];
/* Element = DEP_OBJECT
   Description = Depreciation object */
	char dep_object[1];
/* Element = DEPCLASS
   Description = Depreciation class code */
	char depclass[4];
};
#pragma member_alignment restore
