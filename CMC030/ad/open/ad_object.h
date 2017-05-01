/*
 * File Layout for: AD.AD_OBJECT on 21-May-01
 *
 * Depreciation Object Desription
 */

#pragma member_alignment save
#pragma nomember_alignment

struct ad_object_cdd
{
/* Element = DEP_OBJECT
   Description = Depreciation object */
	char dep_object[1];
/* Element =
   Description = Description */
	char description[20];
};
#pragma member_alignment restore
