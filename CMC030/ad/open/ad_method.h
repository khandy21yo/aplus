/*
 * File Layout for: AD.AD_METHOD on 21-May-01
 *
 * Depreciation Method Description
 */

#pragma member_alignment save
#pragma nomember_alignment

struct ad_method_cdd
{
/* Element = DEP_METHOD
   Description = Depreciation method */
	char dep_method[4];
/* Element =
   Description = Method description */
	char description[40];
/* Element =
   Description = Method calculation code */
	char calculation[2];
};
#pragma member_alignment restore
