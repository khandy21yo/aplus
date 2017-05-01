/*
 * File Layout for: AD.AD_UNITS on 21-May-01
 *
 * Units of Production Journal
 */

#pragma member_alignment save
#pragma nomember_alignment

struct ad_units_cdd
{
/* Element = DEP_OBJECT
   Description = Depreciation object */
	char dep_object[1];
/* Element = DATE
   Description = Date */
	char action_date[8];
/* Element = ASSET_NUM
   Description = Asset number */
	char asset_num[10];
/* Element =
   Description = Units */
	long quantity;
};
#pragma member_alignment restore
