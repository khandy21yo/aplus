/*
 * File Layout for: AD.AD_CONTROL on 21-May-01
 *
 * Asset Depreciation Control File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct ad_control_cdd
{
/* Element = DEP_OBJECT
   Description = Depreciation object to the GL */
	char dep_object[1];
/* Element =
   Description = Last period updated */
	char lastper[6];
};
#pragma member_alignment restore
